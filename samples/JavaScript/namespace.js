(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['lodash'], factory);
  } else if (typeof exports !== 'undefined') {
    module.exports = factory(require('lodash'));
  } else {
    root.Namespace = factory(root._);
  }
})(this, function(_) {
  'use strict';

  /**
   * @module namespace
   * @class namespace
   */
  function Namespace() {}
  
  /**
   * Regex for splitting keypaths into arrays.
   *
   * @private
   * @const {RegExp}
   * @type
   */
  var KEYPATH_SPLITTER = /\./g;
  
  /**
   * An internal cache to avoid calculating a keypath more than once.
   *
   * @private
   * @type {Object}
   */
  var _keypaths = {};
  
  _.extend(Namespace.prototype, {
  
    /**
     * Adds a definition to the namespace object.
     *
     * @public
     * @instance
     * @method add
     * @param {String} keypath - The keypath for the definition to be added at.
     * @param {Function|Object} definition - The definition to be added.
     * @return {Function|Object} - The definition.
     */
    add: function(keypath, definition) {
      return this._walk(keypath, function(memo, name, index, keypath) {
        if (index + 1 === keypath.length) {
          memo[name] = _.extend(definition, memo[name]);
        }
        return memo[name] || (memo[name] = {});
      });
    },
  
    /**
     * Retrieves a definition from the namespace safely.
     *
     * @public
     * @instance
     * @method get
     * @param {String} keypath - The keypath to lookup a definition for.
     * @returns {Function|Object|undefined} - The definition if it exists, otherwise `undefined`.
     */
    get: function(keypath) {
      return this._walk(keypath);
    },
  
    /**
     * An internal function for walking a keypath.
     *
     * @private
     * @instance
     * @method _walk
     * @param {String} keypath - The keypath to walk through.
     * @param {Function} [callback] - An optional callback to be called at each item in the path.
     * @returns {function|Object|undefined} - The reduced keypath.
     */
    _walk: function(keypath, callback) {
      return _.reduce(
        _keypaths[keypath] || (_keypaths[keypath] = keypath.split(KEYPATH_SPLITTER)),
        callback || function(memo, name) {
          return memo && memo[name];
        },
        this
      );
    }
  });
  
  return Namespace;
});

//# sourceMappingURL=namespace.js.map