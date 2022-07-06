package types

// DO NOT EDIT. THIS FILE WAS AUTOMATICALLY GENERATED

// SoftLayer_Container_RemoteManagement_Graphs_SensorSpeed - The
// SoftLayer_Container_RemoteManagement_Graphs_SensorSpeed contains graphs to display speed for each of
// the server's fans. Fan speeds are gathered from the server's remote management card.
type SoftLayer_Container_RemoteManagement_Graphs_SensorSpeed struct {

	// Graph - no documentation
	Graph string `json:"graph,omitempty"`

	// Title - no documentation
	Title string `json:"title,omitempty"`
}

func (softlayer_container_remotemanagement_graphs_sensorspeed *SoftLayer_Container_RemoteManagement_Graphs_SensorSpeed) String() string {
	return "SoftLayer_Container_RemoteManagement_Graphs_SensorSpeed"
}
