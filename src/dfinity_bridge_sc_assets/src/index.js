import { dfinity_bridge_sc } from "../../declarations/dfinity_bridge_sc";

document.getElementById("clickMeBtn").addEventListener("click", async () => {
  const name = document.getElementById("name").value.toString();
  // Interact with dfinity_bridge_sc actor, calling the greet method
  const greeting = await dfinity_bridge_sc.greet(name);

  document.getElementById("greeting").innerText = greeting;
});
