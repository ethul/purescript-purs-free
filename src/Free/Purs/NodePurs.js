'use strict';

var debug_ = require('debug')('Free.Purs.NodePurs');

var execa_ = require('execa');

exports.execa = function execa(cmd) {
  return function (args) {
    return function () {
      var cp = execa_(cmd, args);
      return cp;
    };
  };
};

exports.debug = function debug(format) {
  return function (values) {
    return function () {
      return debug_.apply(debug, [format].concat(values));
    };
  };
};
