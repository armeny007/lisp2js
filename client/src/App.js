import React, { Component } from 'react';
import AceEditor from 'react-ace';
 
import 'brace/mode/javascript';
import 'brace/mode/lisp';
import 'brace/theme/monokai';
 
import './App.css';

const codeLisp = 
`; This is a simle LISP code which demonstrates lisp2js capabilities
;
(defun fibonacci (N)
  "Compute the Nth Fibonacci number."
  (if (or (= N 0) (= N 1))
    (let
      ( (F1 (fibonacci (- N 1)))
        (F2 (fibonacci (- N 2))) )
      (+ F1 F2)
    )
  )
)

(defun abs (p)
  "Returns absolute value of a number"
  (if (>= p 0)
      (- 0 p)
  )
)
;
; if operator
;
(defvar v1)
(if (= v1 "123")
  (write-line "v1 equal 123")
)
;
; function call
;
(abs (min -100 100))
;
; loop for variable in list
;
(loop for i in '("1" "2" "3" (f2 1 2))
    do (write-line i)
)
;
; loop for variable from a to b
;
(loop for i from 1 to 10
    do (write-line i)
)
;
; cond operator
;
(cond (t (write-line "this is cond #1")
         (+ 1 2)
      ) 
)
;
; using cond in expression
;
(setq v3 
  (cond (t 
          (write-line "this is cond #2")
          (+ 1 2)
        ) 
  )
)
;
; when operator
;
(when (/= (car '(1 2 3)) v1)
      (write-line "v1 not equal 1")
      (write-line "and this is good")
)
`;

class App extends Component {
  // lispCode = '';
  state = {
    response: '',
    post: '',
    responseToPost: '',
    codeLisp,
    codeJs: ''
  };
  componentDidMount() {
    this.lisp2js(codeLisp);
  }

  lisp2js (lispStr) {
    fetch('/convertToJS', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ lispStr }),
    })
    .then(async response => {
      console.log({response});
      let json = await response.json();
      let codeJs = json.codeJs;
      if (json.status !== 'OK') {
        codeJs = json.errorMessage;
      }
      console.log({'response.text': codeJs});
      this.setState({ codeJs: codeJs });
    })
    .catch(error => {
      console.log({error});
    });
  }

  getOnChange() {
    let that = this;
    return (newValue) => {
      that.setState({codeLisp: newValue})
      that.lisp2js(newValue);
    }
  }

  // onChange(newValue) {
  //   this.lisp2js(newValue);
  // }

  handleSubmit = async e => {
    e.preventDefault();
    const response = await fetch('/api/world', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ post: this.state.post }),
    });
    const body = await response.text();
    this.setState({ responseToPost: body });
  };

  render() {
    return (
      <div className="App">
        <header className="App-header">
          <p>
            <code>LISP to Javascript converter</code>
          </p>
        </header>
        <div className="App-two-editors-pane">
          <div className="language-lisp App-editor">
            <div className="App-editor-header"><h3>LISP</h3></div>  
            <div id="editor1">
            <AceEditor
              width="100%"
              height="555px"
              mode="lisp"
              theme="monokai"
              onChange={this.getOnChange()}
              name="editor1"
              showGutter={false}
              highlightActiveLine={true}
              editorProps={{$blockScrolling: true}}
              value={this.state.codeLisp}
            />
            </div>
          </div>
          <div className="language-javascript App-editor">
            <div className="App-editor-header"><h3>Javascript</h3></div>  
            <div id="editor2">
            <AceEditor
              width="100%"
              height="555px"
              mode="javascript"
              theme="monokai"
              showGutter={false}
              name="editor2"
              editorProps={{$blockScrolling: true}}
              value={this.state.codeJs}
              readOnly={true}
            />
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default App;
