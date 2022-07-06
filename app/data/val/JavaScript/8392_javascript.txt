// @flow
import React, { Component } from 'react';
import { Grid, Row, Col, FormGroup, FormControl, HelpBlock, ControlLabel } from 'react-bootstrap';


export default class AdminFamilySearch extends Component {
  render() {
    return (
      <div>
        <Grid>
          <Row className="show-grid">
            <Col xs={12} md={8}>
              <form>
                <FormGroup
                  controlId="formBasicText"
                  validationState={() => {}}
                >
                  <ControlLabel><p>Search for family (phone; lastname; lastname, firstname)</p></ControlLabel>
                  <FormControl componentClass="text" />
                  <FormControl.Feedback />
                  <HelpBlock><p>Staff attended check-in station</p></HelpBlock>
                </FormGroup>
              </form>
            </Col>
          </Row>
        </Grid>
      </div>
    );
  }
}
