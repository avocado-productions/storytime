import { Elm } from './App.elm';

const app = Elm.App.init({
  node: document.querySelector('main'),
  flags: { contents: localStorage.getItem('markup') },
});

app.ports.contentsUpdated.subscribe(function(contents: string) {
  localStorage.setItem('markup', contents);
});
