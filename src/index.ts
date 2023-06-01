import "./styles/index.css";
import { Elm } from "./Main.elm";

const elem = document.getElementById("main");
if (elem) {
  const app = Elm.Main.init({ node: elem, flags: undefined });
  app.ports.selectInput.subscribe((id) => {
    const input = document.getElementById(id);
    console.log(input);
    if (input instanceof HTMLInputElement) {
      input.focus();
      input.setSelectionRange(0, 100);
    }
  });
}
