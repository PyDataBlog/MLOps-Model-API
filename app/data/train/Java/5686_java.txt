package edu.towson.cis.cosc603.project2.monopoly.gui;

import edu.towson.cis.cosc603.project2.monopoly.Cell;
import edu.towson.cis.cosc603.project2.monopoly.Player;
import edu.towson.cis.cosc603.project2.monopoly.UtilityCell;

// TODO: Auto-generated Javadoc
/**
 * The Class UtilCellInfoFormatter.
 */
public class UtilCellInfoFormatter extends OwnerName implements CellInfoFormatter {

	/* (non-Javadoc)
	 * @see edu.towson.cis.cosc603.project2.monopoly.gui.CellInfoFormatter#format(edu.towson.cis.cosc603.project2.monopoly.Cell)
	 */
	public String format(Cell cell) {
        UtilityCell c = (UtilityCell)cell;
        StringBuffer buf = new StringBuffer();
        String ownerName = getOwnerName(cell);
        buf.append("<html><b><font color='olive'>")
                .append(cell.getName())
                .append("</font></b><br>")
                .append("$").append(c.getPrice())
				.append("<br>Owner: ").append(ownerName)
                .append("</html>");
        return buf.toString();
	}
}
