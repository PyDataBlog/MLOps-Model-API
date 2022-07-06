---
layout: post
title:  "Altium Designer Design Tipps"
date:   2017-03-06 08:00
categories: Altium-designer
permalink: /posts/altium-designer
---

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/0.AltiumDesigner.png)

# CAD Systeme - Altium Designer Design Tipps
*Einführung und Designtips*



<!--more-->

## Templates

 * Die Templates werden über Preferences &rarr; System &rarr; New Document Defaults eingestellt.
 * PCB Project und Free Documents sollen verlinkt werden.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/1.Templates.png)

## Libraries

 * Die Libraries werden Preferences &rarr; Data Management &rarr; Installed Libraries gewählt.
 * Durch viele lokale Libraries und Libraries am Server arbeitet Altium Langsamer. Daher können sie deaktiviert werden, wenn man sie gerade nicht braucht oder es wird empfohlen SVN zu nutzen und die Kopie lokal abzuspeichern.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/2.Libraries.png)

## Schematic Template

 * In einem Schematic Template können die Texte Automatisch, Local oder Global geändert werden.
   * **Automatischen Wert:** z.B. DrawnDate 
&rarr; wenn man die Seite speichert, bekommt das aktuelle Datum automatisch.
   * **Local Werte:** z.B DrawnName, Checked–Date  und –Name, Module
&rarr; Rechte Maus Taste &rarr; Options &rarr; Document Parameters.
   * **Global Werte:** z.B. Project, Component und DrawingNumber
&rarr; Project Options &rarr; Parameters

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/3.SchematicTemplate.png)

  * Wenn man die Parameters in mehreren Seiten auf Einmal ändern will
    * &rarr; Tools &rarr; Parameter Manager

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/3.SchematicTemplate2.png)

  * Wenn die Parameters geändert wird, wird danach bestätigt.  
    * Accept Changes (Create ECO) &rarr; Execute Changes &rarr; Close

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/3.SchematicTemplate3.png)

## Workspace Panels

  * Unten auf der Rechteseite des Editors.
  * Die Menus unter diese Panels, könnte beliebig ein- und aus- geblendet werden.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/4.WorkspacePanels.png)

### Libraries

  * Die Installierte Libraries werden hier auftauchen. Man kann direkt hier weitere Libraries hinzufügen oder deaktivieren. 
  * Die angelegte Bauteile in der Libraries könnte hier gefunden werden.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/5.WorkspacePanels-Libraries.png)

  * Librarycolumns werden beliebig gewählt.
  * Alle Parameters in Bauteile können als Column dargestellt werden.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/5.WorkspacePanels-Libraries2.png)

### Joker Suche in Libraries

  * Die Bauteilliste kann für jede Column nach der Reihe sortiert und gesucht werden.
  * Wenn man etwas in den Suchbalken ohne „Stern“ schreibt, sucht das Programm nur die Wörter, die am Satzanfang stehen.
  * Setzt man einen Stern (*) vor das gesuchte Wort, so findet er alle Wörter, die in der Library stehen. 
  * Wenn man mehrere Wörter gleichzeitig sucht, setzt man für jedes gesuchte Wort einen Stern davor (*Wort1*, *Wort2*) so findet er alle Wörter, die in der Library stehen.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/6.JockerSuche.png)

### To-Do

  * Wird in Projektdatei gespeichert.
  * Keine extra Datei.
    * &rarr; Recte Maus Taste in To-Do    &rarr; Add Project To-Do Item


![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/7.Todo.png)

  * Man kann die Arbeit zuweisen und Kategorie wählen.
  * Wenn die Arbeit erledigt wird, kann man es abhaken.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/7.Todo2.png)

### Supplier Search

  * Altium sucht Bauteile nur bei Mouser, Digi-Key, Farnell und TME. (Europäische Lieferanten)
  * Newark, ODBC, Arrow und Allied sind auch vorhanden. (nicht Europäische Lieferanten )
  * Die Suche nach folgende Parameter sortiert werden.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/8.SupplierSearch.png)

### Snippets

  * Wiederholte Schaltpläne oder Layouts in Unterschiedliche Projekten könnte in einem Bibliothek gespeichert werden.
  * Die Schaltplanteile können einfach in Snippets Bibliothek hinzugefügt werden.
	  * Selektrieren &rarr; Rechtemaustaste &rarr; Snippets &rarr; Create Snippet from selcted objects

## Netzclass Definieren

  * Die besondere Leitungen oder Pfade könnte man mit Netzclassen spezifiziert.
    * Z.B: man kann Luft und Kriechstrecken, Ströme, HV… definieren.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/9.Netzclass.png)

  * Die Netzclassen könnte unterschiedliche Eigenschaften haben.
  * Unterschiedliche Netzclassen könnte gleiche Eigenschaften haben.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/9.Netzclass2.png)
![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/9.Netzclass3.png)

## text

  * die Taste Einfugen auf einem Text

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/10.Text.png)

  * STRG + Signal klicken

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/10.Text2.png)

## Footprint Manager

  * Das Gleiche Bauteil kann unterschiedliche Footprint haben. Die Anpassung oder Kontrolle werden über Footprint Manager geschafft.
	  * Tools &rarr; footprint Manager
  * Linke Fenster: Bauteile Wählen
  * Rechte Fenster: aktuelle Footprint abhaken.
  * Rechte Maustaste &rarr; Set as Current


![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/11.FootprintManager.png)

## Annotate

  * Wenn es einmal eingestellt wird, kann Annotate Quickly Funktion verwendet werden.
  * Start Index: soll immer von 100 starten.
  * Suffix: zusätzlich Designator- Kennzeichen am Hinten.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/12.Annotate.png)

  * Reset Dupplicate Schematic Designator: Löscht doppel Bauteil Designatornummer.
  
  ![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/12.Annotate2.png)

## Cross Probe

  * Wenn in der PCB Datei Bauteile gesucht werden, wird diese Funktion verwendet. 
  * Tools&rarr; Cross Probe &rarr; wird gesuchte Bauteil selektiert, erscheint in der PCB Datei.
  * Auch von der PCB Datei wird Bauteile im Schaltplan gesucht.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/14.CrossProbe.png)
![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/14.CrossProbe2.png)

## Connection Matrix

  * Erreichbar unter Project &rarr; Project options
  * Einstellung von Sheet Entry, Port und Pin Verbindungen
    * z.B: wenn Input Port mit Input Pin verbindet. Nets with no driving source
  * Default wurde VCC und GND Power, alle andere Pins als Passive dargestellt.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/15.ConnectionMatrix.png)

## Harness

  * Unterschiedliche oder Gleiche Namen
  * Extra Page Connector
  * Bus: Wenn Signal sich wiederholt
	  * Harness Connector: beinhaltet Bus und Harness connectoren.

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/16.Harness.png)

  * Harness kann kaskadiert werden.
  * ein Harness-Connector mehr Mals mit unterschiedlichen Ports verwendet werden

![](https://hakandilek.github.io/layout-pcb.de/static/img/2017-03-06/16.Harness2.png)

## Empfehlungen

  * Schematics Grid soll mindestens auf 5 stehen. Kleiner als 5 ist Gefahr dass die Pins nicht richtig angeschlossen werden. Taste G = Grid Wechseln.
  * Man soll die Bauteilwerte nie im Schematik ändern. Wenn sie geändert werden muss, werden sie nur über Bibliotheken geändert und vom Schaltplan aktualisiert
  * 4 Knoten Punkt soll man nicht verwenden. Wenn muss, dann mit Manuel Junction verwenden.
  * Man kann jeden Parameter vom Bauteil im Schaltplan ein- und aus- blenden.
