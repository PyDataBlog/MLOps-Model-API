import React, {PropTypes} from 'react';
import L from 'leaflet';
import gh from '../api/GitHubApi';

import RaisedButton from 'material-ui/RaisedButton';

const REPO_TIMESPAN = {
    ALLTIME: 0,
    THIRTYDAYS: 1,
    SIXTYDAYS: 2,
    ONEYEAR: 3
};

const defaultMapConfig = {
    options: {
        center: [
            39.7589, -84.1916
        ],
        zoomControl: false,
        zoom: 4,
        maxZoom: 20,
        minZoom: 2,
        scrollwheel: false,
        infoControl: false,
        attributionControl: false
    },
    tileLayer: {
        uri: 'http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',
        options: {
            maxZoom: 18,
            id: ''
        }
    }
};

class RepoUserHeatmap extends React.Component {
    constructor(props, context) {
        super(props, context);

        this.state = {
            timespan: REPO_TIMESPAN.THIRTYDAYS,
            data: []
        };

        this.initializeMap = this.initializeMap.bind(this);
    }

    componentDidMount() {
        this.initializeMap();
        this.updateData();

        gh.getTopRepos().then(data => {
            console.log('=== REPOS ===');
            console.log(data);
            return gh.getContributors(data.data[0].full_name);
        }).then(contribs => {
	    console.log('=== CONTRIBS ===');
            console.log(contribs);
            return gh.getUser(contribs.data[0].login);
	}).then(user => {
            console.log('=== USER ===');
            console.log(user);
            return gh.getRateLimit();
	}).then(limit => {
	    console.log('=== RATE LIMIT ===');
            console.log(limit);
	}).catch(err => {
	    console.log('ERROR:');
	    console.log(err);
	});
    }

    componentWillUnmount() {
        this.map = null;
    }

    initializeMap() {
        if (this.map) {
            return;
        }

        this.map = L.map(this.mapDiv, this.props.mapOptions || defaultMapConfig.options);

        if (this.props.mapLayers && this.props.mapLayers.length > 0) {
            for (let i=0; i < this.props.mapLayers.length; i++) {
                this.props.mapLayers[i].addTo(this.map);
            }
        }
        else {
            L.tileLayer(defaultMapConfig.tileLayer.uri, defaultMapConfig.tileLayer.options).addTo(this.map);
        }
    }

    updateData() {

    }

    render() {
        return (
            <div className="map-container">
                <div className="os-map" ref={(div) => { this.mapDiv = div; }}></div>
                <RaisedButton label="Default" />
            </div>
        );
    }
}

RepoUserHeatmap.propTypes = {
    mapOptions: PropTypes.object,
    mapLayers: PropTypes.array
};

export default RepoUserHeatmap;
