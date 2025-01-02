"use strict";

export function debugLog(object) {
  return function () {
    console.log(object);
  }
}

export function warnLog(object) {
  return function () {
    console.warn(object);
  }
}

export function errorLog(object) {
  return function () {
    console.error(object);
  }
}
