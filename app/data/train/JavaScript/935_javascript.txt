import React from "react"
import Img from "gatsby-image"
import { StaticQuery, graphql } from "gatsby"
import html5 from "../images/html5.svg"
import js from "../images/javascript.svg"
import jQuery from "../images/jquery.svg"
import php from "../images/php.svg"
import python from "../images/python.svg"
import css3 from "../images/css3.svg"
import sass from "../images/sass.svg"
import react from "../images/react.svg"
import redux from "../images/redux.svg"
import angular from "../images/angular.svg"
import nodejs from "../images/nodejs.svg"
import express from "../images/express.svg"
import graphQL from "../images/graphql.svg"
import apollo from "../images/apollo.svg"
import laravel from "../images/laravel.svg"
import django from "../images/django.svg"
import ruby from "../images/ruby.svg"
import rails from "../images/rails.svg"
import firebase from "../images/firebase.svg"
import mongodb from "../images/mongodb.svg"
import postgresql from "../images/postgresql.svg"

const About = ({ id }) => (
  <StaticQuery
    query={graphql`
      query AboutImgQuery {
        aboutImg: file(relativePath: { eq: "towers.jpg" }) {
          childImageSharp {
            fluid(maxWidth: 1200) {
              ...GatsbyImageSharpFluid
            }
          }
        }
      }
    `}
    render={data => (
      <section id={id} className="section cover">
        <Img
          title="About image"
          alt="Towers"
          fluid={data.aboutImg.childImageSharp.fluid}
          style={{
            borderBottom: "2px solid #0F2027",
            position: "absolute",
            left: 0,
            top: 0,
            width: "100%",
            height: "100%",
          }}
        />
        <div className="overlay" />
        <div className="about">
          <h1 className="name mt-5">
            <b>About Me</b>
          </h1>
          <div className="description mb-4">
            <h5 className="greetings">
              I'm a developer who is driven by the motivation to learn and
              utilize all of the <br />
              newest and leading software technologies, tools and frameworks.{" "}
              <br />
              Here are some of the technologies I have worked with:
            </h5>
          </div>
          <div className="svg-container">
            <div className="logo-container">
              <a
                href="https://rebrand.ly/w1zfk5"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={html5} alt="html5" />
              </a>
              <h5>HTML</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/gpe80b"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={css3} alt="css3" />
              </a>
              <h5>CSS</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/ac3zez"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={sass} alt="sass" />
              </a>
              <h5>Sass</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/gdw8nf"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={js} alt="js" />
              </a>
              <h5>JavaScript</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/t8q4kk"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={jQuery} alt="jQuery" />
              </a>
              <h5>jQuery</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/5dmk0k"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={php} alt="php" />
              </a>
              <h5>PHP</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/51v3f7"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={ruby} alt="ruby" />
              </a>
              <h5>Ruby</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/u9f3bu"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={python} alt="python" />
              </a>
              <h5>Python</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/4711zo"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={react} alt="react" />
              </a>
              <h5>React</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/f4fdtb"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={redux} alt="redux" />
              </a>
              <h5>Redux</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/0af3pn"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={angular} alt="angular" />
              </a>
              <h5>Angular</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/fno5hy"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={nodejs} alt="nodejs" />
              </a>
              <h5>Node</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/8pwvla"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={express} alt="express" />
              </a>
              <h5>Express</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/chgco7"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={graphQL} alt="graphQL" />
              </a>
              <h5>GraphQL</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/s8v7qq"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={apollo} alt="apollo" />
              </a>
              <h5>Apollo</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/jm3gu8"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={laravel} alt="laravel" />
              </a>
              <h5>Laravel</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/hbkv6c"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={django} alt="django" />
              </a>
              <h5>Django</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/71jw07"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={rails} alt="rails" />
              </a>
              <h5>Ruby on Rails</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/8jg10f"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={firebase} alt="firebase" />
              </a>
              <h5>Firebase</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/1lamx3"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={mongodb} alt="mongodb" />
              </a>
              <h5>MongoDB</h5>
            </div>
            <div className="logo-container">
              <a
                href="https://rebrand.ly/az0ssm"
                target="_blank"
                rel="noopener noreferrer"
              >
                <img src={postgresql} alt="postgresql" />
              </a>
              <h5>PostgreSQL</h5>
            </div>
          </div>
          <div className="arrow animated bounceInDown" />
        </div>
      </section>
    )}
  />
)

export default About
