"use strict";

exports.executeJavascriptHacks = function () {
  // The popstate event of the Window interface is fired when the active history entry changes while the user navigates the session history. 
  // Hack for making sure hashchanges trigger a reload when nagivating history
  // TODO: handle this event properly without a reload by executing the Initialize action on this event
  window.addEventListener('popstate', () => {
    window.location.reload();
  });

  // Hack to be able to use the right viewport height in the CSS, because the CSS vh value doesn't behave well on mobile.
  var root = document.querySelector(':root');
  window.addEventListener('resize', () => {
    root.style.setProperty('--app-height', vh() + 'px');
  });
  function vh() {
    // - 1 because sometimes the innerHeight doesn't seem to be rounded correctly or something
    return Math.max(document.documentElement.clientHeight, window.innerHeight || 0) - 1;
  }

  const styles = getComputedStyle(document.documentElement);
  const bgColor = styles.getPropertyValue('--header-bg');
  var m = document.createElement('meta');
  m.name = 'theme-color';
  m.content = bgColor;
  document.head.appendChild(m);
};
