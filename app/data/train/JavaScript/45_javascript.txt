import React from 'react';
import './skills.scss';

export default () => {
  return (
    <section className="skills-section">
      <svg className="bigTriangleColor separator-skills" width="100%" height="100" viewBox="0 0 100 102" preserveAspectRatio="none">
          <path d="M0 0 L0 100 L70 0 L100 100 L100 0 Z" />
      </svg>
      <h2 className="skills-header">Skills</h2>
      <p className="skills tlt">
          Javascript/ES6 React Redux Node Express MongoDB GraphQL REST Next.js Mocha Jest JSS PostCSS SCSS LESS AWS nginx jQuery Webpack Rollup UI/Design
      </p>
        <button className="button button--wayra button--inverted skills-btn">
          View My <i className="fa fa-github skills-github"></i><a className="skills-btn-a" href="https://www.github.com/musicbender" target="_blank"></a>
        </button>
    </section>
  );
}
