import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import { Rete, default as init, Production, Condition } from 'rete';

var app;
var rete;

var observer = {};
observer.on_event = msg => {
  const event = JSON.parse(msg);
  console.info('rete event', event);
  var port = (() => { switch (event.type) {
    case 'Initialized':        return app.ports.initialized;
    case 'AddedNode':          return app.ports.addedNode;
    case 'RemovedNode':        return app.ports.removedNode;
    case 'AddedProduction':    return app.ports.addedProduction;
    case 'RemovedProduction':  return app.ports.removedProduction;
    case 'AddedToken':         return app.ports.addedToken;
    case 'RemovedToken':       return app.ports.removedToken;
    case 'AddedWme':           return app.ports.addedWme;
    case 'RemovedWme':         return app.ports.removedWme;
    case 'AddedAlphaMemory':   return app.ports.addedAlphaMemory;
    case 'RemovedAlphaMemory': return app.ports.removedAlphaMemory;
    default:                   return null;
  }})();

  if (port == null) {
    console.warn('Unhandled rete event', event);
  } else {
    port.send(event);
  }
};

async function run() {
  // This file has to be served somehow. It is currently behind a
  // symlink in the public directory.
  await init('/rete_bg.wasm');
  rete = new Rete();
  console.log('rete', rete);

  app = Elm.Main.init({
    node: document.getElementById('root')
  });
  console.log('app', app);  

  rete.observe(observer);
  console.log('observer', observer);
  
  app.ports.addProduction.subscribe(args => {
    console.log('add production', args);
    var production = new Production(args.id);
    args.conditions.forEach(condition => {
      production.add_condition(new Condition(condition.id, condition.attribute, condition.value));
    });
    // const production = args.conditions.reduce((prod, condition) => 
    //   prod.add_condition(new Condition(condition.id, condition.attribute, condition.value)),
    //   new Production(args.id));
    console.log('add production', production);
    rete.add_production(production);
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
    rete.remove_wme(args.timetag);
  });

}
run();

registerServiceWorker();