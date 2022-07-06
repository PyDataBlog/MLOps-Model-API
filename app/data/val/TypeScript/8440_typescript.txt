import * as Common from "cubitt-common";

import {AddedEvent} from "./AddedEvent";
import {EventType} from "./../EventType";

/**
 * An event that is raised when a connector is added.
 */
export class ConnectorAddedEvent extends AddedEvent {
	/**
	 * The RFC4122 v4 compliant ID of the node to which the new connector belongs.
	 */
	public nodeId: Common.Guid;

	/**
	 * @param sourceId The RFC4122 v4 compliant ID of the command that caused this event.
	 * @param version The new current version number.
	 * @param timestamp The timestamp for the moment this event was created in milliseconds elapsed since 1 January 1970 00:00:00 UTC.
	 * @param elementId The RFC4122 v4 compliant ID of the new element.
	 * @param elementType The type of the new element.
	 * @param elementProperties The properties of the new element.
	 * @param nodeId The RFC4122 v4 compliant ID of the node to which the new connector belongs.
	 */
	constructor(
		sourceId: Common.Guid,
		version: number,
		timestamp: number,
		elementId: Common.Guid,
		elementType: string,
		elementProperties: Common.Dictionary<any>,
		nodeId: Common.Guid
	) {
		super(
			sourceId,
			version,
			EventType.ConnectorAdded,
			timestamp,
			elementId,
			elementType,
			elementProperties);
		this.nodeId = nodeId;
	}
}
