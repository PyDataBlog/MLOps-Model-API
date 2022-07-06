/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package clay;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author MultiTool

 Need spring force of repulsion, spring force of attraction
 also need binding radius and un-binding (bond breaking) radius. 

 */
public class Things {
  public static int NDims = 2;
  /* **************************************************************************** */
  public static class Point {
    double[] V = new double[NDims];
    /* **************************************************************************** */
    public double Magnitude() {// pythagoras
      double dif, sumsq = 0.0;
      for (int dcnt = 0; dcnt < NDims; dcnt++) {
        dif = this.V[dcnt];
        sumsq += dif * dif;
      }
      return Math.sqrt(sumsq);
    }
    /* **************************************************************************** */
    public double DeltaMag(Point other) {// pythagoras
      double dif, sumsq = 0.0;
      for (int dcnt = 0; dcnt < NDims; dcnt++) {
        dif = this.V[dcnt] - other.V[dcnt];
        sumsq += dif * dif;
      }
      return Math.sqrt(sumsq);
    }
    /* **************************************************************************** */
    public void DeltaVec(Point origin, Point diff) {
      for (int dcnt = 0; dcnt < NDims; dcnt++) {
        diff.V[dcnt] = this.V[dcnt] - origin.V[dcnt];
      }
    }
    /* **************************************************************************** */
    public void AddVec(Point other) {
      for (int dcnt = 0; dcnt < NDims; dcnt++) {
        this.V[dcnt] += other.V[dcnt];
      }
    }
    /* **************************************************************************** */
    public void Unitize() {
      double length = this.Magnitude();
      for (int dcnt = 0; dcnt < NDims; dcnt++) {
        this.V[dcnt] = this.V[dcnt] / length;
      }
    }
    /* **************************************************************************** */
    public void Multiply(double magnitude) {
      for (int dcnt = 0; dcnt < NDims; dcnt++) {
        this.V[dcnt] *= magnitude;
      }
    }
    /* **************************************************************************** */
    public void Clear() {
      for (int dcnt = 0; dcnt < NDims; dcnt++) {
        this.V[dcnt] = 0.0;
      }
    }
    /* **************************************************************************** */
    public void Copy(Point other) {
      System.arraycopy(other.V, 0, this.V, 0, NDims);
    }
  }
  /* **************************************************************************** */
  public static class SpringVec extends Point {
    public int GenStamp;
  }
  /* **************************************************************************** */
  public static class Link {
    public Atom mine;
    public Link parallel;
    public static double RestingRadius = 10;
    public static double Radius = 5.0;
    public static double BindingRadius = 5.0;
    public static double BreakingRadius = BindingRadius + 1.0;
    public SpringVec Spring;
    // should we have two links between atoms or one? 
    // with a planet/grav model, two gravity wells would fit
    // a spring is symmetrical. 
    public void InterLink(Link other) {
      other.parallel = this;
      this.parallel = other;
      other.Spring = this.Spring = new SpringVec();// to do: rewrite this for single two-way links rather than twin links.
    }
    public Atom GetOtherAtom() {
      return this.parallel.mine;
    }
    public void CalcForceVector(Point Delta) {
      Point diffv;
      double distortion, dif;
      Atom me = this.mine;
      Atom you = this.GetOtherAtom();
      diffv = new Point();
      me.Loc.DeltaVec(you.Loc, diffv);
      dif = diffv.Magnitude();
      distortion = dif - Link.RestingRadius;// distortion is displacement from resting length of spring
      diffv.Unitize();
      diffv.Multiply(-distortion);// to do: since this force is applied once to each atom, the displacement back to resting is wrongly doubled
      // other issue is that we are wastefully calculating the dif vector twice, once for each end of the link pair.
      // a = f/m
      // the link pair COULD have a common storage object that keeps reusable info such as vector. baroque. 

      // get locations of both of my ends
      // get my distortion from my resting length
      // normalize my direction vector, multiply by magnitude of distortion.
    }
  }
  /* **************************************************************************** */
  public static class Atom {
//    public double Radius = 5.0;
//    public double BindingRadius = 5.0;
//    public double BreakingRadius = BindingRadius + 1.0;
    public Point Loc = new Point();
    public Point LocNext = new Point();
    public Point Vel = new Point();
    public Map<Atom, Link> Bindings;
    public Atom() {
      this.Bindings = new HashMap<>();
    }
    public void Bind(Atom other) {
      Link OtherLnk = new Link();
      OtherLnk.mine = other;
      Link MeLnk = new Link();
      MeLnk.mine = this;
      MeLnk.InterLink(OtherLnk);
      this.Bindings.put(other, MeLnk);
      other.Bindings.put(this, OtherLnk);
    }
    public void Rollover() {
      this.Loc.Copy(this.LocNext);
    }
    /* **************************************************************************** */
    public void Seek_Bindings(Atom[] Atoms) {
      Atom you;// ultimately replace this with 2d array-based collision detection.
      double dif;
      int NumAtoms = Atoms.length;
      for (int acnt1 = 0; acnt1 < NumAtoms; acnt1++) {
        you = Atoms[acnt1];
        if (this != you) {
          if (!this.Bindings.containsKey(you)) {// Find out if you are already connected to me. 
            dif = this.Loc.DeltaMag(you.Loc);
            if (dif < Link.BindingRadius) {// if not bound, then bind
              this.Bind(you);
            }
          }
        }
      }
    }
    /* **************************************************************************** */
    public void Seek_Unbindings() {
      Atom YouAtom;
      Link MeLnk;
      double dif;
      Iterator it = this.Bindings.entrySet().iterator();
      while (it.hasNext()) {
        Map.Entry pair = (Map.Entry) it.next();
        MeLnk = (Link) pair.getValue(); // System.out.println(pair.getKey() + " = " + pair.getValue());
        //YouAtom = MeLnk.GetOtherAtom();
        YouAtom = (Atom) pair.getKey();
        dif = this.Loc.DeltaMag(YouAtom.Loc);
        if (dif > Link.BreakingRadius) {// if bound, then break
          // here we remove from my table via iterator, and from your table via remove(key)
          YouAtom.Bindings.remove(this);
          it.remove();
          MeLnk.mine = null;
          MeLnk.parallel.mine = null;
        }
      }
    }
  }
  /* **************************************************************************** */

  public int NumAtoms = 100;
  public Atom[] Atoms;
  public int GenCnt = 0;
  /* **************************************************************************** */
  public Things() {
    Atoms = new Atom[NumAtoms];
  }
  /* **************************************************************************** */
  public void React() {
    GenCnt++;
    Atom me;
    Link MeLnk;
    Point DiffV = new Point();
    for (int acnt0 = 0; acnt0 < NumAtoms; acnt0++) {
      me = this.Atoms[acnt0];
      me.LocNext.Clear();
      Map<Atom, Link> yall = me.Bindings;
      int younum = yall.size();
      for (int acnt1 = 0; acnt1 < younum; acnt1++) {
        MeLnk = yall.get(acnt1);
        if (MeLnk.Spring.GenStamp < GenCnt) {
          MeLnk.CalcForceVector(DiffV);// f=ma but m is always 1 for now
          MeLnk.Spring.Copy(DiffV);
          MeLnk.Spring.GenStamp = GenCnt;
        }
        // to do: gotta fix this so force is going opposite directions for each end of the spring
        me.LocNext.AddVec(MeLnk.Spring);// Accumulate all displacements into my next move.
        //you = MeLnk.GetOtherAtom(); dif = me.Loc.DeltaMag(you.Loc);
        // do physics here
        // spring physics, then define new locations and speeds 
        // phys 0: go through all my neighbors and see which springs are bent. apply force to myself accordingly for each spring.
        // phys 1: after all personal next locs are calculated, then rollover for everybody.
      }
    }
  }
  /* **************************************************************************** */
  public void Rebind() {
    Atom me;
    for (int acnt0 = 0; acnt0 < NumAtoms; acnt0++) {
      me = this.Atoms[acnt0];
      /*
       I can scan all nbrs for new bindings.
       Though I only need to scan my own connections for UNbindings. so I'm already pointing to the link in question when/if I want to break it.
       */
      me.Seek_Unbindings();
      me.Seek_Bindings(this.Atoms);
    }
  }
}
/*
 Set<String> keys = hm.keySet();
 for(String key: keys){
 System.out.println("Value of "+key+" is: "+hm.get(key));
 }

 Enumeration e = ht.elements();

 while (e.hasMoreElements()){
 System.out.println(e.nextElement());
 }

 public static void printMap(Map mp) {// http://stackoverflow.com/questions/1066589/iterate-through-a-hashmap
 Iterator it = mp.entrySet().iterator();
 while (it.hasNext()) {
 Map.Entry pair = (Map.Entry)it.next();
 System.out.println(pair.getKey() + " = " + pair.getValue());
 it.remove(); // avoids a ConcurrentModificationException
 }
 }

 for (Map.Entry<String, Object> entry : map.entrySet()) {
 String key = entry.getKey();
 Object value = entry.getValue();
 // ...
 }
 for (Object value : map.values()) {
 // ...
 }

 */
