'use strict';

require('./Example.purs').main();

if (module.hot) {
  module.hot.accept();
}

console.log('image from entrypoint.js', require('./logo.png'))
