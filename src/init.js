require('Stylesheets.elm');

var elm = require( 'Main' );

var app = elm.Main.embed(document.getElementById('app'));

app.ports.titlePort.subscribe(x => updateTitle(x));

function updateTitle(x) {
    document.title = x === "" ? "Elm SPA Demo" : x + " | Elm SPA Demo";
}

window.addEventListener("load", () => {
    document.querySelector(".loader").remove()
})