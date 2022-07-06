
import { AbstractPokerCommand } from "../../core/commands/AbstractPokerCommand";



export class ClLogoutCommand extends AbstractPokerCommand {
	public static NAME = "ClLogout";
	public static UID = 10262;
	
	constructor() {
		super();
		this.init(2, 2070, ClLogoutCommand.NAME);
	}
}
