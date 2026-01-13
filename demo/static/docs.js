
console.log("CUSTOM DOCS JS 2")

const sections = document.querySelectorAll("section[id]")
const navLinks = document.querySelectorAll('nav a[href^="#"]')
let isNavigating = false


const obs = new IntersectionObserver((entries) => {

  // Pick the most visible intersecting section
  const visible = entries
    .filter(e => e.isIntersecting)
    .sort((a, b) => b.intersectionRatio - a.intersectionRatio);

  if (!visible[0] || !visible[0].target.id) return


  const activeId = visible[0].target.id
  console.log("VISIBLE", activeId)

  if (!isNavigating) {
    highlightNav(activeId)
  }


  // Optional: keep URL in sync without jump
  history.replaceState(null, "", `#${activeId}`);

}, { threshold: 0, rootMargin: "-10% 0px -80% 0px", });

sections.forEach(s => obs.observe(s));

function highlightNav(activeId) {
  console.log("highlightNav", activeId)
  const activeLink = document.querySelector('nav a[href^="#' + activeId + '"]')
  navLinks.forEach(a => a.classList.remove('nav-active'))
  activeLink.classList.add('nav-active')
}

window.addEventListener('popstate', function(event) {
  console.log("popstate", event, window.location.hash)
  isNavigating = true

  if (window.location.hash) {
    highlightNav(window.location.hash.substring(1))
  }
});



// window.addEventListener('scroll', (_event) => {
//   console.log('scroll');
//   isScrolling = true
// })

window.addEventListener('scrollend', (_event) => {
  console.log('scrollend');
  isNavigating = false
});
