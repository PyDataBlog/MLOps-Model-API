package selector;

import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.util.EventObject;

class Barra
  implements AdjustmentListener
{
  SelectorApplet applet;

  public Barra(SelectorApplet applet)
  {
    this.applet = applet;
  }
  public void adjustmentValueChanged(AdjustmentEvent e) {
    Object obj = e.getSource();
    if (obj == this.applet.sbElectrico)
      this.applet.sbElectrico_adjustmentValueChanged(e);
    else if (obj == this.applet.sbMagnetico)
      this.applet.sbMagnetico_adjustmentValueChanged(e);
    else
      this.applet.sbVelocidad_adjustmentValueChanged(e);
  }
}