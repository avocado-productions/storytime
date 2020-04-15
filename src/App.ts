import { Elm } from './Cyoa.elm';

const app = Elm.Cyoa.init({
  node: document.querySelector('main'),
  flags: localStorage.getItem('markup'),
});
