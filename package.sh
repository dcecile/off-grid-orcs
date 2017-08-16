#!/bin/bash
sbt fullOptJS
package="off-grid-orcs-html5.zip"
rm "$package"
7z a "$package" 'index.html' 'target/scala-2.12/off-grid-orcs-opt.js'
cp "$package" ~/Downloads
