import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import { Rete, default as init } from 'rete';

var app = Elm.Main.init({
  node: document.getElementById('root')
});
console.log('app', app);

var rete;

var observer = {};
observer.on_event = msg => {
  const event = JSON.parse(msg);
  switch(event.type) {
    // case "AddedWme":
    //   app.ports.addedWme.send(event);
    //   break;
    case 'Initialized':
      app.ports.initialized.send(event);
      break;
    default:
      console.warn('Unhandled rete event', event);
  }
};

async function run() {
  // This file has to be served somehow. It is currently behind a
  // symlink in the public directory.
  await init('/rete_bg.wasm');
  rete = new Rete();
  console.log('rete', rete);

  rete.observe(observer);
  console.log('observer', observer);
}
run();

app.ports.addProduction.subscribe(args => {
  console.log('add production', args);
  // rete.add_production();
});

app.ports.removeProduction.subscribe(args => {
  console.log('remove prodution', args);
  rete.remove_production(args.id);
});

app.ports.addWme.subscribe(args => {
  console.log('add wme', args);
  rete.add_wme(args.id, args.attribute, args.value);
});

app.ports.removeWme.subscribe(args => {
  console.log('remove wme', args);
  // rete.remove_wme();
});

registerServiceWorker();
