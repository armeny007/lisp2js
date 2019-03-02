let dotenv = require('dotenv'); // loads environment variables from .env file to process.env
dotenv.config();

function debug(...params) {
  if (process.env.DEBUG == 'true') {
    console.log(...params);
  }
}

module.exports = { debug };