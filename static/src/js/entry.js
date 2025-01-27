import { KeplerGlAPI } from './keplergl-api';

let keplerApiInstance = null;

export function initKeplerGl(containerId, mapBoxApiToken, logging = false) {
  keplerApiInstance = new KeplerGlAPI(containerId, mapBoxApiToken, logging);
  return {
    api: keplerApiInstance,
    store: keplerApiInstance.store
  };
}

export function loadDataset(dataset, onFilterFn = null) {
  if (!keplerApiInstance) {
    throw new Error('Kepler.gl API not initialized. Call initKeplerGl first.');
  }
  keplerApiInstance.loadDataset(dataset, onFilterFn);
}

export function removeDataset(datasetId) {
  if (!keplerApiInstance) {
    throw new Error('Kepler.gl API not initialized. Call initKeplerGl first.');
  }
  keplerApiInstance.removeDataset(datasetId);
}

export function clearAllDatasets() {
  if (!keplerApiInstance) {
    throw new Error('Kepler.gl API not initialized. Call initKeplerGl first.');
  }
  keplerApiInstance.clearAllDatasets();
}

export function getDatasets() {
  if (!keplerApiInstance) {
    throw new Error('Kepler.gl API not initialized. Call initKeplerGl first.');
  }
  return keplerApiInstance.getDatasets();
}

export function hasDataset(datasetId) {
  if (!keplerApiInstance) {
    throw new Error('Kepler.gl API not initialized. Call initKeplerGl first.');
  }
  return keplerApiInstance.hasDataset(datasetId);
}
