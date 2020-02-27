import './css/main.css';
import {
  Elm
} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
require("material-components-web/dist/material-components-web.min.js");
require("material-components-web/dist/material-components-web.min.css");

// CONFIG
const storageName = "elm-spa-boilerplate" // key in localStorage
const dbName = "elm-db"

const app = Elm.Main.init({
  flags: generateFlags(),
  node: document.getElementById('elm')
});

registerServiceWorker(); // Don't want service workers? Just comment this line out

// Generate flags to send to our Elm app on intialization
function generateFlags() {
  return {
    timeAppStarted: Date.now(),
    windowSize: {
      width: window.innerWidth,
      height: window.innerHeight
    },
    localStorage: JSON.parse(localStorage.getItem(storageName)) || null,
    db: JSON.parse(localStorage.getItem(dbName)) || null
  };
}


// [To Elm] Send messages thorugh port on change of localStorage
window.addEventListener("storage", event => {
  if (event.storageArea === localStorage){
    if( event.key === storageName) {
      // console.log(event.newValue)
      app.ports.onLocalStorageChange.send(JSON.parse(event.newValue) || null);
    }
    if (event.key == dbName) {
      console.log(event.newValue)
      app.ports.onDbChange.send(JSON.parse(event.newValue) || null);
    }
  } 
  
  
});


// [From Elm] Set localStorage
app.ports.toLocalStorage.subscribe(data => {
  // console.log(data);
  localStorage.setItem(storageName, JSON.stringify(data));
  // app.ports.onLocalStorageChange.send(data);
});

app.ports.toDb.subscribe(data => {
  console.log("TODB:")
  console.log(data);
  localStorage.setItem(dbName, JSON.stringify(data));
  //app.ports.onDbChange.send(data);
});


// [From Elm] Clear localStorage
app.ports.clearLocalStorage.subscribe(() => {
  // console.log("clearing localStorage");
  localStorage.removeItem(storageName);
  // app.ports.onLocalStorageChange.send(null);
});

/* ----- NOTE: TO CONSIDER ----- 
  
  Notice how lines 43 and 50 are commented out. 
  It seems redundant to tell Elm about a change in localStorage that was initially triggered by Elm. 
  However, this may actually be useful! How?
  
  By doing this, we can synchronize state across multiple tabs!  For example, assuming that some sort of user credentials are stored in localStorage...
  if a user signs out on one tab, and we clear the localStorage through Elm in that tab, our app will receive the update in all other tabs and respond appropriately if we send this 'redundant' message!
  
  However, one must be very careful in how you handle changes in localStorage in each page. 
  You might end up creating an infinite loop of messages being passed, or responding to a change in localStorage twice
  or that reason, I've left this 'redundancy' commented out. It shouldn't be needed for most simple applications

*/