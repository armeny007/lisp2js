const { debug } = require('./logging');

class StateMachine {
  // std - State Transition Diagram
  // states - {state1: object, state2: object, ...}
  // startState - beginning state
  constructor(std, states, startState) { 
    this.std = std;   
    this.states = states;
    this.stack = [];    // stack memory
    this.currentState = startState;
  }

  processEvent(event, char) {
    if (!this.std[this.currentState]) {
      throw new Error(`Missing state "${this.currentState}" in STD`);
    }
    const stdRow = this.std[this.currentState]; 
    if (!stdRow[event]) {
      if (!stdRow['default']) {
        throw new Error(`Missing transition and default transition for state "${this.currentState}" and event "${event}" in STD`);
      }
      event = 'default';
    }
    const stdCell = stdRow[event];
    let newState = this.currentState;
    let topStackSymbol = this.getTopStackSymbol();
    if (!stdCell[topStackSymbol]) {
      if (!stdCell['*']) {
        throw new Error(`Missing transition and default transition for state "${this.currentState}", event "${event}" and stack symbol "${topStackSymbol}" in STD`);
      }
      topStackSymbol = '*';
    }
    if (stdCell[topStackSymbol].toState) {
      newState = stdCell[topStackSymbol].toState;
    };
    let oldState = this.currentState;
    this.changeState(newState);
    if (stdCell[topStackSymbol].action) {
      this.actionCallback(oldState, newState, event, char, stdCell[topStackSymbol].action);
    }
    
    const sAction = stdCell[topStackSymbol].SAction;
    if (sAction) {
      if (!sAction[0] || !sAction[1]) {
        throw new Error(`SAction should be an array of 2 elements`);
      }
      if (sAction[0] == 'push') {
        this.pushToStack(stdCell[topStackSymbol].SAction[1]);
      }
      else if (sAction[0] == 'pop') {
        this.popFromStack();
      }
      else {
        throw new Error(`SAction[0] should be "push" or "pop"`);
      }
    }
  }

  getTopStackSymbol() {
    if (this.stack.length == 0) {
      return '#';
    }
    else {
      return this.stack[this.stack.length - 1];
    }
  }  

  changeState(newState) {
    if (newState != this.currentState) {
      debug(`${this.currentState} -> ${newState}`);
      this.processOnExit(this.currentState);
      this.processOnEnter(newState);
      this.currentState = newState;
    }
  }

  processOnEnter(state) {
    // overload if you need to process onEnter for some states
  }

  processOnExit(state) {
    // overload if you need to process onExit for some states
  }

  pushToStack(symbol) {
    // overload if you need to process push to stack
    this.stack.push(symbol);
    debug(`push "${symbol}" to stack`);
  }

  popFromStack() {
    // overload if you need to process push to stack
    this.stack.pop();
  }

  actionCallback(currentState, newState, event, char, action) {
    // overload if you need to process action for some std cells
  }

  getCurrentState() {
    return this.states[this.currentState];
  }

  getStateByName(state) {
    return this.states[state];
  }

  // char == null means EOF
  charToEvent(char) {
    throw new Error('Abstract method charToEvent should be overloaded');
  }

  parse(str) {
    for (let char of str) {
      const event = this.charToEvent(char);
      debug({char});
      debug({event});
      this.processEvent(event, char);
    }
    this.processEvent(this.charToEvent(null), null);
  }
}

module.exports = {StateMachine}