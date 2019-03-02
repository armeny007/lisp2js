const express = require('express');
const bodyParser = require('body-parser');
const path = require('path');

const { lisp2js } = require('./lib/lisp2js');

const app = express();
const port = process.env.PORT || 5001;
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));

app.post('/isValidLisp', (req, res) => {
  console.log(req.body);
  let lispStr = req.body.lispStr;
  try {
    lisp2js(lispStr);
    res.json({status:'OK'});
  }
  catch (e) {
    res.json({status:'Error'} );
  }
});

app.post('/convertToJS', (req, res) => {
  let lispStr = req.body.lispStr;
  try {
    const js = lisp2js(lispStr);
    res.json({status:'OK', errorMessage: '', codeJs: js});
  }
  catch (e) {
    res.json({status:'Error', errorMessage: e.message, codeJs: ''});
  }
});

if (process.env.NODE_ENV === 'production') {
  // Serve any static files
  app.use(express.static(path.join(__dirname, 'client/build')));
  // Handle React routing, return all requests to React app
  app.get('*', function(req, res) {
    res.sendFile(path.join(__dirname, 'client/build', 'index.html'));
  });
}

app.listen(port, () => console.log(`Listening on port ${port}`));