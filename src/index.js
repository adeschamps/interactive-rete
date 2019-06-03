import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import { Rete } from 'rete';

// var rete = new Rete();

var app = Elm.Main.init({
  node: document.getElementById('root')
});

console.log('app', app);

app.ports.addProduction.subscribe(args => {
  console.log('add production', args);
});

app.ports.removeProduction.subscribe(args => {
  console.log('remove prodution', args);
});

app.ports.addWme.subscribe(args => {
  console.log('add wme', args);
  rete.add_wme(args.id, args.attribute, args.value);
});

app.ports.removeWme.subscribe(args => {
  console.log('remove wme', args);
});

registerServiceWorker();
