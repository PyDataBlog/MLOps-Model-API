/**
  @file Export all functions in yuv-video to user
  @author Gilson Varghese<gilsonvarghese7@gmail.com>
  @date 13 Oct, 2016
**/

/**
  Module includes
*/

var frameReader = require(./lib/framereader); 
var frameWriter = require(./lib/framewriter);
var frameConverter = require(./lib/frameconverter);
/**
  Global variables
*/

var version = "1.0.0";

/**
  Export all the functions to global namespace
*/

module.exports = {
  /**
   Test function to test the reader
  */
  version: function() {
    return version;
  },

  /**
    Frame reader to read frame according to the given options
  */
  frameReader: frameReader,

  /**
    Frame Writer to write frame according to the options
  */
  frameWrite: frameWriter,
  
  /**
    Frame Converter to conver frame into various formats
    Currently only YV21 and V210 are supported
  */
  frameConverter: frameConverter

};
