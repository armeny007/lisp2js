const { LispStateMachine } = require('./lisp-state-machine');
const { debug } = require('./logging');

function lisp2lists(lispStr) {
  const stateMachine = new LispStateMachine();
  stateMachine.parse(lispStr);
  const lists = stateMachine.lists; 
  // console.dir({lists},{depth:null});
  return lists;
}

function isValidLispStr(lispStr) {
  try {
    lisp2lists(lispStr);
    return true;
  }
  catch (e) {
    debug(e.message);
    return false;
  }
}
module.exports = {lisp2lists, isValidLispStr};