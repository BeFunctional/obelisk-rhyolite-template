import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { createStore, combineReducers, applyMiddleware, compose } from 'redux';
import { KeplerGl } from '@kepler.gl/components';
import { keplerGlReducer, filterDatasetCPU } from '@kepler.gl/reducers';
import { addDataToMap, updateVisState } from '@kepler.gl/actions';
import { enhanceReduxMiddleware } from '@kepler.gl/reducers';
import { processCsvData } from '@kepler.gl/processors';
import { StyleSheetManager } from 'styled-components';
import { AutoSizer } from 'react-virtualized';

export class KeplerGlAPI {
  constructor(containerId, mapBoxApiToken, enableLogging = false) {
    this.enableLogging = enableLogging;
    this.log('Initializing KeplerGlAPI');
    this.reducer = null;
    this.filterCallbacks = new Map();
    this.hasWrappedReducer = false;

    try {
      const container = document.getElementById(containerId);
      if (!container) {
        const error = `Container element with id '${containerId}' not found`;
        this.logError('Constructor', error);
        throw new Error(error);
      }

      this.root = ReactDOM.createRoot(container);
      this.reducer = keplerGlReducer;
      this.log('Root container created', { containerId });

      const reducers = combineReducers({
        keplerGl: keplerGlReducer
      });

      const middleWares = enhanceReduxMiddleware([]);
      const enhancers = applyMiddleware(...middleWares);
      this.store = createStore(reducers, {}, compose(enhancers));
      this.log('Redux store initialized');

      this.initialize(mapBoxApiToken);
    } catch (error) {
      this.logError('Constructor', error);
      throw error;
    }
  }

  log(message, data = null) {
    if (this.enableLogging) {
      const timestamp = new Date().toISOString();
      const logMessage = `[KeplerGlAPI ${timestamp}] ${message}`;
      if (data) {
        console.log(logMessage, data);
      } else {
        console.log(logMessage);
      }
    }
  }

  logError(method, error) {
    if (this.enableLogging) {
      const timestamp = new Date().toISOString();
      console.error(
        `[KeplerGlAPI ERROR ${timestamp}] ${method}:`,
        error instanceof Error ? error.message : error
      );
      if (error instanceof Error && error.stack) {
        console.error(`Stack trace:`, error.stack);
      }
    }
  }

  logWarning(method, message, data = null) {
    if (this.enableLogging) {
      const timestamp = new Date().toISOString();
      console.warn(`[KeplerGlAPI WARNING ${timestamp}] ${method}:`, message, data || '');
    }
  }

  initialize(mapBoxApiToken) {
    try {
      this.log('Initializing Kepler element');
      const KeplerElement = () => {
        return React.createElement(
          'div',
          { className: 'h-full w-full' },
          React.createElement(AutoSizer, null, ({ width, height }) =>
            React.createElement(KeplerGl, {
              id: 'map',
              width: width,
              height: height,
              mapboxApiAccessToken: mapBoxApiToken,
            })
          )
        );
      };

      const app = React.createElement(
        StyleSheetManager,
        { disableVendorPrefixes: true },
        React.createElement(
          Provider,
          { store: this.store },
          React.createElement(KeplerElement, null)
        )
      );

      this.root.render(app);
      this.log('Kepler element initialized and rendered');
    } catch (error) {
      this.logError('initialize', error);
      throw error;
    }
  }

  initFilterReducer() {
    if (!this.hasWrappedReducer) {
      const originalReducer = this.store.getState().keplerGl;
      const wrappedReducer = (state = originalReducer, action) => {
        const newState = this.reducer(state, action);

        if (action.type.includes('FILTER')) {
          this.filterCallbacks.forEach((callback, datasetId) => {
            const dataset = newState.map.visState.datasets[datasetId];
            if (dataset) {
              this.log('Dataset filter change detected', { datasetId });
              const filteredDataIdxs = dataset.filteredIndexForDomain;
              callback(filteredDataIdxs);
            }
          });
        }

        return newState;
      };

      this.store.replaceReducer(combineReducers({
        keplerGl: wrappedReducer
      }));

      this.hasWrappedReducer = true;
      this.log('Filter reducer initialized');
    }
  }

  loadDataset(dataset, onFilterFn = null) {
    try {
      if (!dataset) {
        console.log('Dataset is required');
        throw new Error('Dataset is required');
      }

      if (!dataset.label || !dataset.id || !dataset.data) {
        console.log('Dataset missing required properties (label, id, or data)');
        throw new Error('Dataset missing required properties (label, id, or data)');
      }

      this.log('Loading dataset', {
        label: dataset.label,
        id: dataset.id,
        dataSize: dataset.data.length
      });

      this.store.dispatch(addDataToMap({
        datasets: [{
          info: {
            label: dataset.label,
            id: dataset.id
          },
          data: processCsvData(dataset.data)
        }],
        config: {
          mapStyle: { styleType: 'light' }
        },
        option: {
          centerMap: true,
          keepExistingConfig: true
        }
      }));

      this.log('Dataset loaded successfully', { datasetId: dataset.id });

      if (onFilterFn !== null) {
        const debounce = (callback, wait) => {
          let timeoutId = null;
          return (...args) => {
            window.clearTimeout(timeoutId);
            timeoutId = window.setTimeout(() => {
              callback(...args);
            }, wait);
          };
        };

        this.filterCallbacks.set(dataset.id, debounce(onFilterFn, 50));
        this.initFilterReducer();
        this.log('Dataset filter callback registered', { datasetId: dataset.id });
      }

    } catch (error) {
      console.log('Error while loading dataset:', error);
      this.logError('loadDataset', error);
      throw error;
    }
  }

  removeDataset(datasetId) {
    try {
      if (!datasetId) {
        throw new Error('Dataset ID is required');
      }

      this.log('Removing dataset', { datasetId });

      const state = this.store.getState();
      const currentDatasets = state.keplerGl.map.datasets;

      if (!currentDatasets[datasetId]) {
        this.logWarning('removeDataset', `Dataset with ID '${datasetId}' not found`);
        return;
      }

      const newState = {
        ...state.keplerGl.map,
        datasets: Object.fromEntries(
          Object.entries(currentDatasets)
            .filter(([id]) => id !== datasetId)
        ),
        layers: state.keplerGl.map.layers
          .filter(layer => layer.config.dataId !== datasetId)
      };

      this.store.dispatch({
        type: '@@kepler.gl/UPDATE_MAP_STATE',
        payload: newState
      });

      this.filterCallbacks.delete(datasetId);
      this.log('Dataset removed successfully');
    } catch (error) {
      this.logError('removeDataset', error);
      throw error;
    }
  }

  clearAllDatasets() {
    try {
      this.log('Clearing all datasets');
      const state = this.store.getState();
      const currentDatasets = Object.keys(state.keplerGl.map.datasets);

      this.log('Current state before clearing', {
        datasetsCount: currentDatasets.length,
        datasetIds: currentDatasets,
        layersCount: state.keplerGl.map.layers.length
      });

      const newState = {
        ...state.keplerGl.map,
        datasets: {},
        layers: []
      };

      this.store.dispatch({
        type: '@@kepler.gl/UPDATE_MAP_STATE',
        payload: newState
      });

      this.filterCallbacks.clear();
      this.log('All datasets cleared successfully');
    } catch (error) {
      this.logError('clearAllDatasets', error);
      throw error;
    }
  }

  getDatasets() {
    try {
      this.log('Retrieving datasets');
      const state = this.store.getState();
      const datasets = state.keplerGl.map.datasets;

      this.log('Datasets retrieved', {
        count: Object.keys(datasets).length,
        datasetIds: Object.keys(datasets)
      });

      return datasets;
    } catch (error) {
      this.logError('getDatasets', error);
      throw error;
    }
  }

  hasDataset(datasetId) {
    try {
      if (!datasetId) {
        throw new Error('Dataset ID is required');
      }

      this.log('Checking for dataset', { datasetId });
      const datasets = this.getDatasets();
      const exists = datasets.hasOwnProperty(datasetId);

      this.log('Dataset check result', {
        datasetId,
        exists,
        availableDatasets: Object.keys(datasets)
      });

      return exists;
    } catch (error) {
      this.logError('hasDataset', error);
      throw error;
    }
  }

  setLogging(enabled) {
    try {
      const previousState = this.enableLogging;
      this.enableLogging = enabled;
      console.log(
        `[KeplerGlAPI ${new Date().toISOString()}] Logging ${enabled ? 'enabled' : 'disabled'} ` +
        `(previous state: ${previousState ? 'enabled' : 'disabled'})`
      );
    } catch (error) {
      console.error('[KeplerGlAPI] Error while setting logging state:', error);
    }
  }
}
