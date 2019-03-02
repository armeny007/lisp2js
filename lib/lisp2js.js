const beautify = require('js-beautify').js;

const { lisp2lists } = require('./lisp2lists');
const { debug } = require('./logging');

const BUILTIN_FUNCTIONS = ['write', 'write-line', 'min', 'max', 'car', 'cdr'];

// List of Lists should be converted into Scope tree
class Scope {
  constructor(globalScope, parentScope, name) {
    this.globalScope = globalScope;
    this.parentScope = parentScope;
    this.name = name;
    this.variableChain = [];
    this.functionChain = [];
    this.bodyChain = [];
    this.beforeEOL = '';
  }

  isLocalFunction(name) {
    for (let i = 0; i < this.functionChain.length; i++) {
      let functionName = this.functionChain[i].name;
      if (functionName == name) {
        return true;
      }
    }
    return false;
  }

  isGlobalFunction(name) {
    return this.globalScope ? this.globalScope.isLocalFunction(name) : this.isLocalFunction(name); // globalScope has this.globalScope = null;
  }

  getGlobalScope() {
    return this.globalScope ? this.globalScope : this;
  }

  addEOL() {
    if (this.beforeEOL) {
      this.bodyChain.push(this.beforeEOL);
      this.beforeEOL = '';
    }
    this.bodyChain.push('\n');
  }

  addBeforeEOL(str) {
    this.beforeEOL += str;
  }

  getVariable(variableName) {
    let globalScope = this.getGlobalScope();
    let scope = this;
    while (scope) {
      for (let i = 0; i < scope.variableChain.length; i++) {
        let variable = scope.variableChain[i];
        if (typeof variable == 'object') { // scope
          if (variable.name == variableName) {
            return variable;
          }
        }
      }
      scope = scope.parentScope;
    }
    return null;
  }
}

function lisp2js(lispStr) {
  const lists = lisp2lists(lispStr);
  const globalScope = new Scope(null, null, '');
  for (var i = 0; i < lists.length; i++) {
    var list = lists[i];
    // generate scope tree
    list2scope(list, 'block', globalScope);
  }
  // when scopes tree is generated we need just to walk through the tree and concat 
  // all the source code parts into one sctring.
  let js = generateJs(globalScope);
  // prettify the result
  js = beautify(js, { indent_size: 2, space_in_empty_paren: true });
  return js;
}

function returnPrefix(parentScope, shouldReturn) {
  if (shouldReturn) {
    parentScope.bodyChain.push('return ');
  }
}

function returnSuffix(parentScope, shouldReturn, list) {
  checkElementComment(list, parentScope);
  if (shouldReturn) {
    parentScope.bodyChain.push(';');
    parentScope.addEOL();
  }
}

function checkElementComment(list, parentScope) {
  if (list && list.comment) {
    parentScope.addBeforeEOL('\t//' + list.comment);
  }
}

// list2scope  is the main list processing function. 
// it recursively walks though the lists and generates scope tree
function list2scope(list, role, parentScope, onTopOfScope, shouldReturn, onTopOfExpression) {
  if (onTopOfScope && list.value.length == 0 && list.type != 'comment') {
    parentScope.bodyChain.push('// ');
    if (list.type == 'list') {
      parentScope.bodyChain.push('()');
    }
    parentScope.addEOL();
    return;
  }
  if (list.type == 'atom') {
    if (!list.value) {
      parentScope.addEOL();
    }
    else {
      if (onTopOfScope && !shouldReturn) {
        parentScope.bodyChain.push('// ');
      }
      returnPrefix(parentScope, shouldReturn);
      let atom = convertLispConstToJs(list.value);
      parentScope.bodyChain.push(atom);
      returnSuffix(parentScope, shouldReturn, list)
      if (onTopOfScope && !shouldReturn) {
        parentScope.addEOL();
      }
    }
  }

  if (list.type == 'comment') {
    parentScope.bodyChain.push('//' + list.value);
    parentScope.addEOL();
  }

  if (list.type == 'string') {
    if (onTopOfScope && !shouldReturn) {
      parentScope.bodyChain.push('// ');
    }
    returnPrefix(parentScope, shouldReturn);
    parentScope.bodyChain.push("'" + list.value.replace(/[\']/g, '\\\'') + "'");
    returnSuffix(parentScope, shouldReturn, list)
    if (onTopOfScope && !shouldReturn) {
      parentScope.addEOL();
    }
  }

  if (list.type == 'list') {
    if (role == 'simple-list') {
      returnPrefix(parentScope, shouldReturn);
      processListAsSimpleList(list, parentScope);
      returnSuffix(parentScope, shouldReturn)
    }
    else if (role == 'first-is-atom') {
      if (list.quoted) {
        processListAsQuotedList(list, parentScope, shouldReturn, onTopOfScope);
      }
      else {
        // const currentScope = new Scope(parentScope.globalScope, parentScope);
        const firstAtom = getFirstAtom(list);
        checkElementComment(list.value[0], parentScope);
        switch (firstAtom) {
          case 'defun': processListAsDefun(list, parentScope, shouldReturn); break;
          case 'defvar': processListAsDefvar(list, parentScope, shouldReturn); break;
          case 'setq': processListAsSetq(list, parentScope, shouldReturn); break;
          case 'cond': processListAsCond(list, parentScope, shouldReturn, onTopOfScope); break;
          case 'if': processListAsIf(list, parentScope, shouldReturn); break;
          case 'when': processListAsWhen(list, parentScope, shouldReturn, onTopOfScope); break;
          case 'loop': processListAsLoop(list, parentScope, shouldReturn); break;
          case 'let': processListAsLet(list, parentScope, shouldReturn); break;
          // case 't': processListAsT(list, parentScope, shouldReturn, onTopOfScope); break;
          // case 'nil': processListAsNil(list, parentScope, shouldReturn, onTopOfScope); break;
          case '+':
          case '-':
          case '*':
          case '/':
          case '=':
          case '/=':
          case '<':
          case '>':
          case '<=':
          case '>=':
          case 'and':
          case 'or':
            // returnPrefix(parentScope, shouldReturn);
            // if (!onTopOfExpression) { parentScope.bodyChain.push('('); }
            processListAsDoubleSideOperator(list, parentScope, shouldReturn, onTopOfScope, onTopOfExpression);
            // if (!onTopOfExpression) { parentScope.bodyChain.push(')'); }
            // returnSuffix(parentScope, shouldReturn);
            break;
          case 'not':
            // returnPrefix(parentScope, shouldReturn);
            processListAsSingleSideOperator(list, parentScope, shouldReturn, onTopOfScope, onTopOfExpression);
            // returnSuffix(parentScope, shouldReturn);
            break;
          // processListArithmeticOperator(list, parentScope); break;
          default: returnPrefix(parentScope, shouldReturn);
            processListAsFunctionCall(list, parentScope, shouldReturn);
            if (shouldReturn) {
              returnSuffix(parentScope, shouldReturn);
            }
            else if (onTopOfScope) {
              parentScope.bodyChain.push(';');
              parentScope.addEOL();
            }
        }
      }
    }
    else if (role == 'block') {
      processListAsBlock(list, parentScope, 0);
    }
    else {
      throw new Error(`Wrong list role: "${role}"`);
    }
  }
  // parentScope.bodyChain.push(currentScope);
}

function generateJsFromChain(chain) {
  let js = '';
  for (let i = 0; i < chain.length; i++) {
    let chainEl = chain[i];
    let type = typeof chainEl;
    if (typeof chainEl == 'object') { // scope
      js += generateJs(chainEl);
    }
    else if (typeof chainEl == 'string') {
      js += chainEl;
    }
  }
  return js;
}

function generateJs(scope) {
  let js = '';
  js += generateJsFromChain(scope.variableChain);
  js += js ? '\n' : '';
  js += generateJsFromChain(scope.functionChain);
  js += js ? '\n' : '';
  js += generateJsFromChain(scope.bodyChain);

  return js;
}

function getFirstAtom(list) {
  if (list.value.length == 0) {
    throw new Error('Unexpected empty list');
  }
  if (list.value[0].type != 'atom') {
    throw new Error('First list element should be atom');
  }
  return list.value[0].value;
}

function processListAsSimpleList(list, parentScope) {
  for (let i = 0; i < list.value.length; i++) {
    let listEl = list.value[i];
    list2scope(listEl, 'first-is-atom', parentScope);
    if (i < list.value.length - 1) {
      parentScope.bodyChain.push(', ');
    }
  }
}

function processListAsBlock(list, parentScope, fromIndex, toIndex, shouldReturn) {
  if (!toIndex) {
    toIndex = list.value.length - 1;
  }
  else {
    toIndex = Math.min(toIndex, list.value.length - 1);
  }

  for (let i = fromIndex; i <= toIndex; i++) {
    let listEl = list.value[i];
    let elementShouldReturn = false;
    if (i == toIndex && shouldReturn) {
      elementShouldReturn = true;
    }
    list2scope(listEl, 'first-is-atom', parentScope, true, elementShouldReturn, true);
  }
}

function generateFunctionParameters(list, parentScope) {
  for (let i = 1; i < list.value.length; i++) { // from second element
    let listEl = list.value[i];
    list2scope(listEl, 'first-is-atom', parentScope, false, false, true);
    if (i < list.value.length - 1) {
      parentScope.bodyChain.push(', ');
    }
  }
}

function generateFunctionCall(name, list, parentScope) {
  parentScope.bodyChain.push(name + '(');
  generateFunctionParameters(list, parentScope)
  parentScope.bodyChain.push(')');
}

function generateArrayMethodCall(originaName, name, list, parentScope, paramsStr) {
  let secondListEl = list.value[1];
  if (!secondListEl) {
    throw new Error(`Missing argument for ${originaName}`)
  }
  if (list.value.length > 2) {
    throw new Error(`Too many arguments for ${originaName}`)
  }
  // if (!secondListEl.quoted) {
  //   parentScope.bodyChain.push(name + '(');
  // }
  list2scope(secondListEl, 'first-is-atom', parentScope, false, false, true);
  // if (!secondListEl.quoted) {
  //   parentScope.bodyChain.push(')');
  // }
  parentScope.bodyChain.push(`.${name}(${paramsStr})`);
}

function processAsBuiltInFunction(name, list, parentScope) {
  switch (name) {
    case 'write': generateFunctionCall('process.stdout.write', list, parentScope); return;
    case 'write-line': generateFunctionCall('console.log', list, parentScope); return;
    case 'min': generateFunctionCall('Math.min', list, parentScope); return;
    case 'max': generateFunctionCall('Math.max', list, parentScope); return;
    case 'car': generateArrayMethodCall('car', 'slice', list, parentScope, '0, 1'); return;
    case 'cdr': generateArrayMethodCall('car', 'slice', list, parentScope, '1'); return;
    default: throw new Error(`"${name}" is not a built in function`)
  }
}

function processListAsDefvar(list, parentScope) {
  var secondListEl = list.value[1];
  var thirdListEl = list.value[2];
  if (!secondListEl) {
    throw new Error('Missing variable name after defvar');
  }
  if (secondListEl.type != 'atom') {
    throw new Error(`Unexpected ${secondListEl.type} after defvar`);
  }
  if (list.value.length > 3) {
    throw new Error(`Too many parameters for defvar`);
  }
  let variableName = secondListEl.value;
  let variable = parentScope.getVariable(variableName);
  if (variable) {
    throw new Error(`Variable ${variableName} already exists`);
  }
  let globalScope = parentScope.getGlobalScope();
  let scope = new Scope(parentScope.getGlobalScope, parentScope, variableName);
  globalScope.variableChain.push(scope);
  if (globalScope == parentScope) {
    parentScope.bodyChain.push('var ' + variableName);
    if (!thirdListEl) {
      parentScope.bodyChain.push(';');
      parentScope.addEOL();
    }
  }
  else {
    globalScope.variableChain.push('var ' + variableName + ';\n');
    if (thirdListEl) {
      parentScope.bodyChain.push(variableName);
    }
  }
  checkElementComment(secondListEl, parentScope);
  if (thirdListEl) {
    parentScope.bodyChain.push(' = ');
    list2scope(thirdListEl, 'first-is-atom', parentScope, false);
    parentScope.bodyChain.push(';');
    parentScope.addEOL();
  }
}

function processListAsSetq(list, parentScope) {
  var secondListEl = list.value[1];
  var thirdListEl = list.value[2];
  if (!secondListEl) {
    throw new Error('Missing variable name after setq');
  }
  if (secondListEl.type != 'atom') {
    throw new Error(`Unexpected ${secondListEl.type} after setq`);
  }
  if (list.value.length > 3) {
    throw new Error(`Too many parameters for setq`);
  }
  let variableName = secondListEl.value;
  let variable = parentScope.getVariable(variableName);
  if (!variable) {
    parentScope.bodyChain.push('let ');
  }
  parentScope.bodyChain.push(variableName);
  checkElementComment(secondListEl, parentScope);
  parentScope.bodyChain.push(' = ');
  list2scope(thirdListEl, 'first-is-atom', parentScope, false, false, true);
  parentScope.bodyChain.push(';');
  parentScope.addEOL();

  let scope = new Scope(parentScope.getGlobalScope, parentScope, variableName);
  parentScope.variableChain.push(scope);
}

function checkListContainsOnlyType(list, msg, type) {
  if (list.type != 'list') {
    throw new Error(`Unexpected ${list.type} instead of list in ${msg}`);
  }
  for (let i = 0; i < list.value.length; i++) {
    let listEl = list.value[i];
    if (listEl.type != type) {
      throw new Error(`Unexpected ${listEl.type} in ${msg}`);
    }
  }
}

function checkListContainsOnlyAtoms(list, msg) {
  checkListContainsOnlyType(list, msg, 'atom');
}

function checkListContainsOnlyLists(list, msg) {
  checkListContainsOnlyType(list, msg, 'list');
}

function processListAsDefun(list, parentScope) {
  var secondListEl = list.value[1];
  var thirdListEl = list.value[2];
  if (!secondListEl) {
    throw new Error('Missing function name after defun');
  }
  if (secondListEl.type != 'atom') {
    throw new Error(`Unexpected ${secondListEl.type} after defun`);
  }
  if (!thirdListEl) {
    throw new Error('Missing function parameters after defun');
  }
  if (thirdListEl.type != 'list') {
    throw new Error(`Unexpected ${thirdListEl.type} after function name`);
  }
  const functionName = secondListEl.value;
  let scope = new Scope(parentScope.getGlobalScope(), parentScope, functionName);
  parentScope.functionChain.push(scope);
  scope.bodyChain.push('function ' + functionName + '(');
  checkElementComment(secondListEl, parentScope);
  checkListContainsOnlyAtoms(thirdListEl, `"${functionName}" function params`);
  list2scope(thirdListEl, 'simple-list', scope);
  scope.bodyChain.push(') {');
  scope.addEOL();
  if (list.value.length > 3) {
    if (list.value.length > 4) {
      processListAsBlock(list, scope, 3, list.value.length - 2); // skip last list in block
    }
    // scope.bodyChain.push('return ');
    let lastEl = list.value[list.value.length - 1];
    list2scope(lastEl, 'first-is-atom', scope, true, true, true);
  }
  scope.bodyChain.push('}');
  scope.addEOL();
}

function isBuiltInFunction(name) {
  return BUILTIN_FUNCTIONS.includes(name);
}

function processListAsFunctionCall(list, parentScope) {
  let firstAtom = getFirstAtom(list);
  if (parentScope.isLocalFunction(firstAtom) || parentScope.isGlobalFunction(firstAtom)) {
    generateFunctionCall(firstAtom, list, parentScope);
  }
  else if (isBuiltInFunction(firstAtom)) {
    processAsBuiltInFunction(firstAtom, list, parentScope);
  }
  else {
    throw new Error(`Unknown function name ${firstAtom}`);
  }
}

function processListAsIf(list, parentScope, shouldReturn) {
  var secondListEl = list.value[1];
  var thirdListEl = list.value[2];
  var fourthListEl = list.value[3];
  if (!secondListEl) {
    throw new Error('Missing condition after if');
  }
  if (!thirdListEl) {
    throw new Error('Missing action1 list after if condition');
  }
  if (list.value.length > 4) {
    throw new Error('Too many parameters in if condition');
  }
  parentScope.bodyChain.push('if (');
  list2scope(secondListEl, 'first-is-atom', parentScope, false, false, true);
  parentScope.bodyChain.push(') {');
  parentScope.addEOL();
  list2scope(thirdListEl, 'first-is-atom', parentScope, true, shouldReturn);
  parentScope.bodyChain.push('}');
  parentScope.addEOL();
  if (fourthListEl) {
    parentScope.bodyChain.push('else {');
    parentScope.addEOL();
    list2scope(fourthListEl, 'first-is-atom', parentScope, true, shouldReturn);
    parentScope.bodyChain.push('}');
    parentScope.addEOL();
  }
}

// code generated by cond in case of onTopOfScope
// (_ => {
//   if (cond1) {
//     action1.1;
//     actoin1.2;
//     return action1.3;
//   }
//   else if (cond2) {
//     action2.1;
//     actoin2.2;
//     return action2.3;
//   }
//    else return null;
// })()
function processListAsCond(list, parentScope, shouldReturn, onTopOfScope) {
  var secondListEl = list.value[1];
  if (!secondListEl) {
    throw new Error('Missing (condition action1 action2 ...) after cond');
  }
  for (var i = 1; i < list.value.length; i++) {
    var listEl = list.value[i]; // each listEl should be a pair: condition - action
    var conditionEl = listEl.value[0];
    var actionEl = listEl.value[1];
    if (!conditionEl) {
      throw new Error('Missing condition in (condition action1 action2 ...) in cond');
    }
    if (!actionEl) {
      throw new Error('Missing action1 in (condition action1 action2 ...) in cond');
    }
    if (onTopOfScope) {
      if (i == 1) {
        parentScope.bodyChain.push('if (');
      }
      else {
        parentScope.bodyChain.push('else if (');
      }
      list2scope(conditionEl, 'first-is-atom', parentScope, false, false, true);
      parentScope.bodyChain.push(') {');
      parentScope.addEOL();
      processListAsBlock(listEl, parentScope, 1, listEl.value.length - 1, shouldReturn);
      parentScope.addEOL();
      parentScope.bodyChain.push('}');
      parentScope.addEOL();
      if (i == list.value.length - 1) {
        if (shouldReturn) {
          parentScope.bodyChain.push('else return null;');
          parentScope.addEOL();
        }
      }
    }
    else {
      if (i == 1) {
        parentScope.bodyChain.push('(_ => {')
        parentScope.addEOL();
        parentScope.bodyChain.push('if (');
      }
      else {
        parentScope.bodyChain.push('else if (');
      }
      list2scope(conditionEl, 'first-is-atom', parentScope, false, false, true);
      parentScope.bodyChain.push(') {');
      parentScope.addEOL();
      processListAsBlock(listEl, parentScope, 1, listEl.value.length - 1, true);
      parentScope.bodyChain.push('}');
      parentScope.addEOL();
      if (i == list.value.length - 1) {
        parentScope.bodyChain.push('else return null;');
        parentScope.addEOL();
        parentScope.bodyChain.push('})()');
      }
    }
  }
}

function processListAsWhen(list, parentScope, shouldReturn, onTopOfScope) {
  var conditionEl = list.value[1];
  if (!conditionEl) {
    throw new Error('Missing condition after when');
  }
  if (onTopOfScope) {
    parentScope.bodyChain.push('if (');
    list2scope(conditionEl, 'first-is-atom', parentScope, false, false, true);
    parentScope.bodyChain.push(') {');
    parentScope.addEOL();
    processListAsBlock(list, parentScope, 2, list.value.length - 1, shouldReturn);
    parentScope.addEOL();
    parentScope.bodyChain.push('}');
    parentScope.addEOL();
    if (shouldReturn) {
      parentScope.bodyChain.push('else return null;');
      parentScope.addEOL();
    }
  }
  else {
    parentScope.bodyChain.push('(_ => {');
    parentScope.addEOL();
    parentScope.bodyChain.push('if (');
    list2scope(conditionEl, 'first-is-atom', parentScope, false, false, true);
    parentScope.bodyChain.push(') {');
    parentScope.addEOL();
    processListAsBlock(list, parentScope, 2, list.value.length - 1, true);
    parentScope.bodyChain.push('}');
    parentScope.addEOL();
    parentScope.bodyChain.push('else return null;');
    parentScope.addEOL();
    parentScope.bodyChain.push('})()');
  }
}

function processListAsVarDefinition(list, parentScope) {
  var firstAtom = getFirstAtom(list);
  var secondListEl = list.value[1];
  // if (!secondListEl) {
  //   throw new Error('Missing value for variable ${firstAtom} definition in let');
  // }
  if (list.value.length > 2) {
    throw new Error(`Too many parameters for variable ${firstAtom} definition in let`);
  }
  let variableName = firstAtom;
  parentScope.bodyChain.push('let ' + variableName);
  checkElementComment(list.value[0], parentScope);
  if (secondListEl) {
    parentScope.bodyChain.push(' = ');
    list2scope(secondListEl, 'first-is-atom', parentScope, false);
  }
  else {
    parentScope.bodyChain.push('null');
  }
  parentScope.bodyChain.push(';');
  parentScope.addEOL();

}

function processListAsLet(list, parentScope, shouldReturn) {
  var secondListEl = list.value[1];
  var thirdListEl = list.value[2];
  if (!secondListEl) {
    throw new Error('Missing variable definition list after let');
  }
  if (!thirdListEl) {
    throw new Error('Missing action1 after variable definition list in let');
  }
  checkListContainsOnlyLists(secondListEl, `let variable definition list`);

  for (let i = 0; i < secondListEl.value.length; i++) {
    let varDefEl = secondListEl.value[i];
    processListAsVarDefinition(varDefEl, parentScope);
  }
  processListAsBlock(list, parentScope, 2, list.value.length - 1, shouldReturn);
  // list2scope(thirdListEl, 'first-is-atom', parentScope, true, shouldReturn, true);
}

// function processListAsLoopUntilReturn(list, parentScope, shouldReturn) {
//   var thirdListEl = list.value[2];
// }

function processListAsLoopFor(list, parentScope, shouldReturn) {
  var thirdListEl = list.value[2]; // loop variable
  var forthListEl = list.value[3]; // in or from
  var fifthListEl = list.value[4]; // list of values | start value
  var sixthListEl = list.value[5]; // do keyword     | to
  var seventhListEl = list.value[6]; // action       | end value
  var eighthListEl = list.value[7]; //               | do keyword
  var ninthListEl = list.value[8]; //                | action
  if (!thirdListEl) {
    throw new Error('Missing loop variable name after loop for');
  }
  if (thirdListEl.type != 'atom') {
    throw new Error(`Unexpected ${thirdListEl.type} after loop for`);
  }
  if (!forthListEl || forthListEl.type != 'atom' || !['in', 'from'].includes(forthListEl.value)) {
    throw new Error('Missing "in" or "from" keyword after variable name');
  }
  let variableName = thirdListEl.value;
  if (forthListEl.value == 'in') {
    if (!fifthListEl || fifthListEl.type != 'list') {
      throw new Error('Missing list of variable values after loop for in');
    }
    if (!sixthListEl || sixthListEl.type != 'atom' || sixthListEl.value != 'do') {
      throw new Error('Missing "do" keyword after variable values list in loop for in');
    }
    if (!seventhListEl) {
      throw new Error('Missing action list in loop for in');
    }
    parentScope.bodyChain.push(`for (let ${variableName} of [`);
    checkElementComment(thirdListEl, parentScope);
    checkElementComment(forthListEl, parentScope);
    list2scope(fifthListEl, 'simple-list', parentScope, false);
    checkElementComment(sixthListEl, parentScope);
    parentScope.bodyChain.push(`]) {`);
    list2scope(seventhListEl, 'first-is-atom', parentScope, true);
    parentScope.bodyChain.push(`}`);
    parentScope.addEOL();
  }
  else { // from
    if (!fifthListEl) {
      throw new Error('Missing start value after loop for from');
    }
    if (!sixthListEl || sixthListEl.type != 'atom' || sixthListEl.value != 'to') {
      throw new Error('Missing "to" keyword after in loop for from');
    }
    if (!seventhListEl) {
      throw new Error('Missing end value after loop for from');
    }
    if (!eighthListEl || eighthListEl.type != 'atom' || eighthListEl.value != 'do') {
      throw new Error('Missing "do" keyword after in loop for from');
    }
    if (!ninthListEl) {
      throw new Error('Missing action list in loop for from');
    }

    parentScope.bodyChain.push(`for (let ${variableName} = `);
    checkElementComment(thirdListEl, parentScope);
    checkElementComment(forthListEl, parentScope);
    list2scope(fifthListEl, 'first-is-atom', parentScope, false);
    checkElementComment(sixthListEl, parentScope);
    parentScope.bodyChain.push(`; ${variableName} <= `);
    list2scope(seventhListEl, 'first-is-atom', parentScope, false);
    parentScope.bodyChain.push(`; ${variableName}++ ) {`);
    checkElementComment(eighthListEl, parentScope);
    list2scope(ninthListEl, 'first-is-atom', parentScope, true);
    parentScope.bodyChain.push(`}`);
    parentScope.addEOL();
  }

}

function processListAsLoop(list, parentScope, shouldReturn) {
  var secondListEl = list.value[1];
  if (!secondListEl) {
    throw new Error('Missing loop parameters');
  }
  if (secondListEl.type == 'atom' && secondListEl.value == 'for') {
    checkElementComment(secondListEl, parentScope);
    processListAsLoopFor(list, parentScope, shouldReturn);
  }
  else {
    throw new Error('Missing keyword "for" after loop');
    // processListAsLoopUntilReturn(list, parentScope, shouldReturn);
  }
}

function convertLispOperatorToJs(operator) {
  let js = operator;
  switch (operator) {
    case '=': js = '=='; break;
    case '/=': js = '!='; break;
    case 'and': js = '&&'; break;
    case 'or': js = '||'; break;
    case 'not': js = '!'; break;
  }
  return js;
}

function convertLispConstToJs(atom) {
  let js = atom;
  switch (atom) {
    case 't': js = 'true'; break;
    case 'nil': js = 'null'; break;
  }
  return js;
}

function processListAsDoubleSideOperator(list, parentScope, shouldReturn, onTopOfScope, onTopOfExpression) {
  if (onTopOfScope && !shouldReturn) {
    parentScope.bodyChain.push('// ');
  }
  returnPrefix(parentScope, shouldReturn);
  if (!onTopOfExpression) { parentScope.bodyChain.push('('); }
  var operator = getFirstAtom(list);
  var secondListEl = list.value[1];
  var thirdListEl = list.value[2];
  if (!secondListEl) {
    throw new Error(`Missing first operand after "${operator}" operator`);
  }
  if (!thirdListEl) {
    throw new Error(`Missing second operand after "${operator}" operator`);
  }
  operator = convertLispOperatorToJs(operator);
  list2scope(secondListEl, 'first-is-atom', parentScope);
  parentScope.bodyChain.push(` ${operator} `);
  list2scope(thirdListEl, 'first-is-atom', parentScope);
  if (!onTopOfExpression) { parentScope.bodyChain.push(')'); }
  returnSuffix(parentScope, shouldReturn);
  if (onTopOfScope && !shouldReturn) {
    parentScope.addEOL();
  }
}

function processListAsSingleSideOperator(list, parentScope, shouldReturn, onTopOfScope, onTopOfExpression) {
  if (onTopOfScope && !shouldReturn) {
    parentScope.bodyChain.push('// ');
  }
  returnPrefix(parentScope, shouldReturn);
  var operator = getFirstAtom(list);
  var secondListEl = list.value[1];
  if (!secondListEl) {
    throw new Error(`Missing operand after "${operator}" operator`);
  }
  operator = convertLispOperatorToJs(operator);
  parentScope.bodyChain.push(` ${operator} `);
  // parentScope.bodyChain.push('(');
  list2scope(secondListEl, 'first-is-atom', parentScope, false, false, false);
  // parentScope.bodyChain.push(')');
  returnSuffix(parentScope, shouldReturn);
}

// function processListAsT(list, parentScope, shouldReturn, onTopOfScope) {
//   if (onTopOfScope && !shouldReturn) {
//     parentScope.bodyChain.push('// ');
//   }
//   if (shouldReturn) {
//     parentScope.bodyChain.push('return ');
//   }
//   parentScope.bodyChain.push('true');
//   if (shouldReturn) {
//     parentScope.bodyChain.push(';');
//     parentScope.addEOL();
//   }
// }

// function processListAsNil(list, parentScope, shouldReturn, onTopOfScope) {
//   if (onTopOfScope && !shouldReturn) {
//     parentScope.bodyChain.push('// ');
//   }
//   if (shouldReturn) {
//     parentScope.bodyChain.push('return ');
//   }
//   parentScope.bodyChain.push('null');
//   if (shouldReturn) {
//     parentScope.bodyChain.push(';');
//     parentScope.addEOL();
//   }
// }

function processListAsQuotedList(list, parentScope, shouldReturn, onTopOfScope) {
  if (onTopOfScope && !shouldReturn) {
    parentScope.bodyChain.push('// ');
  }
  returnPrefix(parentScope, shouldReturn);
  parentScope.bodyChain.push('[');
  for (var i = 0; i < list.value.length; i++) {
    var listEl = list.value[i]; // assume that all nested lists are quoted too
    list2scope(listEl, 'first-is-atom', parentScope, false, false, false);
    if (i < list.value.length - 1) {
      parentScope.bodyChain.push(', ');
    }
  }
  parentScope.bodyChain.push(']');
  returnSuffix(parentScope, shouldReturn);
  if (onTopOfScope && !shouldReturn) {
    parentScope.addEOL();
  }
}


module.exports = { lisp2js };