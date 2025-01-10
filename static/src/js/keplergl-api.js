import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { createStore, combineReducers, applyMiddleware, compose } from 'redux';
import { KeplerGl } from '@kepler.gl/components';
import { keplerGlReducer } from '@kepler.gl/reducers';
import { addDataToMap } from '@kepler.gl/actions';
import { enhanceReduxMiddleware } from '@kepler.gl/reducers';
import { StyleSheetManager } from 'styled-components';

export class KeplerGlAPI {
  constructor(containerId, enableLogging = false) {
    this.enableLogging = enableLogging;
    this.log('Initializing KeplerGlAPI');

    try {
      const container = document.getElementById(containerId);
      if (!container) {
        const error = `Container element with id '${containerId}' not found`;
        this.logError('Constructor', error);
        throw new Error(error);
      }

      this.root = ReactDOM.createRoot(container);
      this.log('Root container created', { containerId });

      const reducers = combineReducers({
        keplerGl: keplerGlReducer
      });

      const middleWares = enhanceReduxMiddleware([]);
      const enhancers = applyMiddleware(...middleWares);
      this.store = createStore(reducers, {}, compose(enhancers));
      this.log('Redux store initialized');

      this.initialize();
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

  initialize() {
    try {
      this.log('Initializing Kepler element');
      const KeplerElement = (props) => {
        return React.createElement(
          'div',
          { style: { position: 'relative', left: 0, width: '100vw', height: '100vh' } },
          React.createElement(KeplerGl, {
            id: 'map',
            width: props.width || 1200,
            height: props.height || 800
          })
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

  loadDataset(dataset) {
    try {
      if (!dataset) {
        throw new Error('Dataset is required');
      }

      if (!dataset.label || !dataset.id || !dataset.data) {
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
          data: dataset.data
        }],
        options: {
          centerMap: true
        }
      }));

      this.log('Dataset loaded successfully', { datasetId: dataset.id });
    } catch (error) {
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
