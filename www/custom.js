Shiny.addCustomMessageHandler('click', function(id) {
  // pequeño delay para esperar a que el UI y los inputs se rendericen
  setTimeout(function() {
    var el = document.getElementById(id);
    if (el) el.click();
  }, 220); // 200-300 ms suele ser suficiente
});



// Mostrar u ocultar el botón al hacer scroll
window.onscroll = function() {
  let btn = document.getElementById("scrollTopBtn");
  if (document.body.scrollTop > 300 || document.documentElement.scrollTop > 300) {
    btn.style.display = "block";
  } else {
    btn.style.display = "none";
  }
};

// Función para volver al inicio suavemente
document.addEventListener("DOMContentLoaded", function() {
  let btn = document.getElementById("scrollTopBtn");
  btn.addEventListener("click", function() {
    window.scrollTo({ top: 0, behavior: "smooth" });
  });
});
