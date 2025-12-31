
console.log("CUSTOM DOCS JS")

const sections = document.querySelectorAll("section[id]")
const navLinks = document.querySelectorAll('nav a[href^="#"]')

console.log("SECTIONS", sections, navLinks)

// const sections = links
//   .map(a => document.querySelector(a.getAttribute("href")))
//   .filter(Boolean);

// console.log("SECTIONS", links)

// const byId = new Map(links.map(a => [a.getAttribute("href").slice(1), a]));

const obs = new IntersectionObserver((entries) => {

  // Pick the most visible intersecting section
  const visible = entries
    .filter(e => e.isIntersecting)
    .sort((a, b) => b.intersectionRatio - a.intersectionRatio);

  if (!visible[0] || !visible[0].target.id) return

  const activeId = visible[0].target.id
  const activeLink = document.querySelector('nav a[href^="#' + activeId + '"]')

  console.log("VISIBLE", activeId, activeLink.href)

  navLinks.forEach(a => a.classList.remove('nav-active'))
  activeLink.classList.add('nav-active')

  // Optional: keep URL in sync without jump
  // history.replaceState(null, "", `#${id}`);
}, { threshold: 0, rootMargin: "-10% 0px -80% 0px", });

sections.forEach(s => obs.observe(s));
