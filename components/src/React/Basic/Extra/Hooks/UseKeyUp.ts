exports.getKeyImpl = (just: (v: Number) => Number) => (
  nothing: Number
) => (event: { keyCode: Number }) =>
  event.keyCode ? just(event.keyCode) : nothing;
