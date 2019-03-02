var expect    = require("chai").expect;
var { lisp2js } = require("../lib/lisp2js");

describe("LISP to Javascript converter", function() {
  describe("Gets javascript code", function() {
    it("converts constats", function() {
      let sT = lisp2js('(write-line t)');
      let sNil = lisp2js('(write-line nil)');

      expect(sT).to.equal("console.log(true);");
      expect(sNil).to.equal("console.log(null);");
    });
    
    it("converts quoted lists", function() {
      let sQL = lisp2js("(write-line '(a b (1 2 3)))");
      expect(sQL).to.equal("console.log(['a', 'b', [1, 2, 3]]);");
    });
  });

  describe("Gets lisp syntax error", function() {
    it("Gets error for wrong lists", function() {
      it("Checks unclosed list", function() {
        let sUnclosed = lisp2js('((() () ())');

        expect(sUnclosed).to.equal("Error: Open list at the end of file");  
      })
    });
  });
});