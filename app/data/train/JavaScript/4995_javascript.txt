/* eslint no-console: 0 */
'use strict';

const fs = require('fs');
const mkdirp = require('mkdirp');

const rollup = require('rollup');

const nodeResolve = require('rollup-plugin-node-resolve');
const commonjs = require('rollup-plugin-commonjs');
const uglify = require('rollup-plugin-uglify');

const src = 'src';
const dest = 'dist/rollup-aot';

Promise.all([
  // build main/app
  rollup.rollup({
    entry: `${src}/main-aot.js`,
    context: 'this',
    plugins: [
      nodeResolve({ jsnext: true, module: true }),
      commonjs(),
      uglify({
        output: {
          comments: /@preserve|@license|@cc_on/i,
        },
        mangle: {
          keep_fnames: true,
        },
        compress: {
          warnings: false,
        },
      }),
    ],
  }).then(app =>
    app.write({
      format: 'iife',
      dest: `${dest}/app.js`,
      sourceMap: false,
    })
  ),

  // build polyfills
  rollup.rollup({
    entry: `${src}/polyfills-aot.js`,
    context: 'this',
    plugins: [
      nodeResolve({ jsnext: true, module: true }),
      commonjs(),
      uglify(),
    ],
  }).then(app =>
    app.write({
      format: 'iife',
      dest: `${dest}/polyfills.js`,
      sourceMap: false,
    })
  ),

  // create index.html
  new Promise((resolve, reject) => {
    fs.readFile(`${src}/index.html`, 'utf-8', (readErr, indexHtml) => {
      if (readErr) return reject(readErr);
      const newIndexHtml = indexHtml
        .replace('</head>', '<script src="polyfills.js"></script></head>')
        .replace('</body>', '<script src="app.js"></script></body>');

      mkdirp(dest, mkdirpErr => {
        if (mkdirpErr) return reject(mkdirpErr);
        return true;
      });

      return fs.writeFile(
        `${dest}/index.html`,
        newIndexHtml,
        'utf-8',
        writeErr => {
          if (writeErr) return reject(writeErr);
          console.log('Created index.html');
          return resolve();
        }
      );
    });
  }),
]).then(() => {
  console.log('Rollup complete');
}).catch(err => {
  console.error('Rollup failed with ', err);
});
