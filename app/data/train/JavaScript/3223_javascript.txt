exports.translate = function(tag) {
  return this.import("riot").compile(tag);
};
