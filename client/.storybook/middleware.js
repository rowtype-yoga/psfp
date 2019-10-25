const express = require('express')
const request = require('request');

const expressMiddleWare = router => {
  router.use('/api', (req, res) => {
    var url = "http://localhost:14188" + req.url;
    req.pipe(request({ qs:req.query, uri: url, json: true })).pipe(res);
  })
}

module.exports = expressMiddleWare
