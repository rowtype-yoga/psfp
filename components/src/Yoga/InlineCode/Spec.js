exports.newInputEvent = s => {
  return new Event("input", { data: s, bubbles: true });
};

exports.newChangeEvent = new Event("change");
