var expect    = require("chai").expect;
var request = require("request");
let dotenv = require('dotenv'); // loads environment variables from .env file to process.env
dotenv.config();

var { lisp2js } = require("../lib/lisp2js");

describe("LISP to Javascript converter API", function() {
  let serverUrl = process.env.SERVER_URL;
  let validUrl = serverUrl + '/isValidLisp';
  let convertUrl = serverUrl + '/convertToJS';
  describe("Gets javascript code", function() {
    it("returns status code 200", function(done) {
      request.post(convertUrl, {form: {lispStr: '(write t)'}}, function(error, response, body) {
        expect(response.statusCode).to.equal(200);
        done();
      });
    });
    it("converts t constat", function(done) {
      request.post(convertUrl, {form: {lispStr: '(write-line t)'}}, function(error, response, body) {
        expect(JSON.parse(body).codeJs).to.equal("console.log(true);");
        done();
      });
    });
  });

  describe("Gets lisp syntax error", function() {
    describe("Gets error for wrong lists", function() {
      it("Checks unclosed list", function(done) {
        request.post(convertUrl, {form: {lispStr: '(((a b) (c) (d e f))'}}, function(error, response, body) {
          expect(JSON.parse(body).status).to.equal('Error');
          expect(JSON.parse(body).errorMessage).to.equal('Error: Open list at the end of file');
          done();
        });
      })
    });
  });

  describe("Checks if LISP code is correct", function() {
    it("returns status code 200", function(done) {
      request.post(validUrl, {form: {lispStr: '(write t)'}}, function(error, response, body) {
        expect(response.statusCode).to.equal(200);
        done();
      });
    });
    it("returns OK for correct lisp", function(done) {
      request.post(validUrl, {form: {lispStr: '(write t)'}}, function(error, response, body) {
        expect(JSON.parse(body).status).to.equal('OK');
        done();
      });
    });
    it("returns Error for incorrect lisp", function(done) {
      request.post(validUrl, {form: {lispStr: 'write t'}}, function(error, response, body) {
        expect(JSON.parse(body).status).to.equal('Error');
        done();
      });
    });
  })
});