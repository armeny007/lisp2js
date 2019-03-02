const { lisp2js } = require('./lisp2js');

const lisp = 
`
'(1 2 3)
`;

const js = lisp2js(lisp);
console.log('js:');
console.log(js);