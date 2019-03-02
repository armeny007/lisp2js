const {lisp2lists, isValidLispStr} = require('./lisp2lists');

const valid = isValidLispStr(' \n\
;comment long "\n\
(+ 1 2)\n\
()\n\
'
);

console.log({valid});