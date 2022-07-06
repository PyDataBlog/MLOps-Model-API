
import { ArrayBufferBuilder } from "../../../../utils/ArrayBufferBuilder";
import { BitsReader } from "../../../../utils/BitsReader";
import { SerializerUtils } from "../../../../utils/SerializerUtils";
import { SrvStartRebuyGracePeriodData } from "../data/SrvStartRebuyGracePeriodData";
import { PokerSeatGraceData } from "../data/PokerSeatGraceData";

import { PokerSeatGraceDataSerializer } from "../serializers/PokerSeatGraceDataSerializer";



export class SrvStartRebuyGracePeriodDataSerializer {
	public static serialize(buffer: ArrayBufferBuilder, data: SrvStartRebuyGracePeriodData): void {
		PokerSeatGraceDataSerializer.serialize(buffer, data.graceVO);
			
	}

	public static deserialize(buffer: ArrayBufferBuilder, data: SrvStartRebuyGracePeriodData): void {
		data.graceVO = new PokerSeatGraceData(data); 
                        PokerSeatGraceDataSerializer.deserialize(buffer, data.graceVO);
			
	}
}
