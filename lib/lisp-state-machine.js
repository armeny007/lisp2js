const { STD } = require('./lisp-std');
const { StateMachine } = require('./state-machine');

const { debug } = require('./logging');

const LISP_STATES = [ 'list', 'atom', 'string', 'comment', 'quote', 'end' ];

// converts LISP program into List of Lists array
class LispStateMachine extends StateMachine {
  constructor () {
    super(STD, LISP_STATES, 'list');

    this.currentList = {type: 'list', parent: null, value: []}; // root
    this.lists = [this.currentList];  //List of Lists array
    this.buffer = '';
    this.lastNodeOnCurrentLine = null;
  }

  // char == null means EOF
  charToEvent(char) {
    let eolRe = /[\r\n]/;
    let whitespaceRe = /[\s]/; // except eol characters

    if (char == null) {
      return 'EOF';
    }
    if (eolRe.test(char)) {
      return 'EOL';
    }
    if (['(',')',';','"', '\''].includes(char)) {
      return char;
    }
    if (whitespaceRe.test(char)) {
      return 'whitespace';
    }
    return 'char-or-digit';
  }

  processOnExit(stateName) {
    switch (stateName) {
      case 'atom': this.addAtomNodeToList(); break;
      case 'string': this.addStringNodeToList(); break;
      case 'comment': this.addCommentNodeToList(); break;
    }
  }

  pushNodeToList(list, node) {
    list.value.push(node);
    if (node.type != 'comment' && node.type != 'list') {
      this.lastNodeOnCurrentLine = node;
    }
  }

  pushNodeToCurrentList(node) {
    this.pushNodeToList(this.currentList, node);
  }

  addAtomNodeToList() {
    debug(`add atom to list "${this.buffer}"`);
    let node = {type: 'atom', parent: this.currentList, value: this.buffer};
    node.quoted = this.currentList && this.currentList.quoted;
    if (node.quoted && node.value && isNaN(parseFloat(node.value))) {
      node.type = 'string';
    } 
    this.pushNodeToCurrentList(node);
    this.buffer = '';
  }

  addStringNodeToList() {
    debug(`add string to list "${this.buffer}"`);
    let node = {type: 'string', parent: this.currentList, value: this.buffer};
    node.quoted = this.currentList && this.currentList.quoted;
    this.pushNodeToCurrentList(node);
    this.buffer = '';
  }

  addCommentNodeToList() {
    debug(`add comment to list "${this.buffer}"`);
    if (this.lastNodeOnCurrentLine) {
      this.lastNodeOnCurrentLine.comment = this.buffer
      this.buffer = '';
    }
    else {
      let node = {type: 'comment', parent: this.currentList, value: this.buffer};
      this.pushNodeToCurrentList(node);
      this.buffer = '';
    }
  }

  actionCallback(currentState, newState, event, char, action) {
    let actionName = null;
    let actionParam = '';
    if (action[0]) {
      actionName = action[0];
    }
    if (action[1]) {
      actionParam = action[1];
    }
    switch (actionName) {
      case 'error': throw new Error('Error: ' + actionParam);
      case 'addList': this.addList(); break;
      case 'addQuotedList': this.addQuotedList(); break;
      case 'appendCharToBuffer': this.appendCharToBuffer(char); break;
      case 'newLine': this.newLine(); break;
    }
  }

  newLine() {
    this.lastNodeOnCurrentLine = null;
  }

  appendCharToBuffer(char) {
    debug(`append to buffer ${char}`);
    this.buffer += char;
  }

  addList() {
    let parent = this.currentList;
    let list = {type: 'list', parent, value: []};
    list.quoted = parent && parent.quoted;
    this.currentList = list;
    this.pushNodeToList(parent, list)
    // parent.value.push(list);
  }

  addQuotedList() { 
    let parent = this.currentList;
    let list = {type: 'list', parent, value: [], quoted: true};
    this.currentList = list;
    this.pushNodeToList(parent, list)
    // parent.value.push(list);
  }

  popFromStack(symbol) {
    debug('pop from stack');
    this.stack.pop();
    let parent = this.currentList.parent;
    this.currentList = parent;
  }


}

module.exports = { LispStateMachine };