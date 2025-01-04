'use strict';

/**
 * 
 * @param {ArrayBuffer} arrayBuffer
 * @returns {Promise<AudioBuffer>}
 */
export const array2AudioBufferImpl = (arrayBuffer) => {
  const audioContext = new AudioContext();
  return audioContext.decodeAudioData(arrayBuffer);
};
