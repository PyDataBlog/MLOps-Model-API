
import { ArrayBufferBuilder } from "../../../../utils/ArrayBufferBuilder";
import { PokerCommandHeaderSerializer } from "../../core/serializers/PokerCommandHeaderSerializer";
import { PokerCommandHeader } from "../../core/data/PokerCommandHeader";
import { ClUnsubscribeFromJackpotsCommand } from "../commands/ClUnsubscribeFromJackpotsCommand";


export class ClUnsubscribeFromJackpotsCommandSerializer {
	public static serialize(buffer: ArrayBufferBuilder, command: ClUnsubscribeFromJackpotsCommand ): void {
		buffer.pointer += PokerCommandHeader.HEADER_SIZE_BYTES;
		
		PokerCommandHeaderSerializer.serialize(buffer, command.header);
	}

	public static deserialize(buffer: ArrayBufferBuilder, command: ClUnsubscribeFromJackpotsCommand ): void {
		PokerCommandHeaderSerializer.deserialize(buffer, command.header);
		
	}
}
