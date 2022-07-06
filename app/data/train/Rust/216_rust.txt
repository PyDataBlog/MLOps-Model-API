#[macro_use] extern crate nickel;
extern crate tera;
extern crate postgres;

use postgres::{Connection, SslMode};
use nickel::{Nickel, HttpRouter, StaticFilesHandler, FormBody};

use tera::{Tera, Context};
use std::collections::HashMap;
//use std::str;

fn main() {
    //Initialize nickel instance
    let mut server: Nickel = Nickel::new();

    //Enable static file handling
    server.utilize(StaticFilesHandler::new("/var/www/ninetytwo/static/"));

    //set tera root
    let templates = Tera::new("/var/www/ninetytwo/templates/*");


    //DB Models
    struct Message{
        _id: u32,
        body: String,
        email: String,
    }

    macro_rules! page {
        ($view_name: ident, $context: ident) => (
            let html_path = str::replace(stringify!($view_name), "_", "/");
            let template_name = html_path.to_string() + ".html";
            let html: String = templates.render(template_name.as_str(), $context).unwrap();

            if stringify!($view_name) == "index"{
                server.get("/", middleware!(html.as_str()));
            }
            else{
                let path = str::replace(stringify!($view_name), "_", "/");
                let route = "/".to_string() + path.as_str();

                server.get(route, middleware!(html.as_str()));
            }
        )
    }

    //Routing

    //Home Page
    let mut index_data = HashMap::new();
    index_data.insert("name", "ninetytwo");
    index_data.insert("description", "The source code for this website.");
    index_data.insert("langs", "HTML | CSS | JavaScript | Rust");
    index_data.insert("link", "https://github.com/ninetytwo/ninetytwo.ml");
    let projects = [index_data];
    let mut index_context = Context::new();
    index_context.add("projects", &projects);
    page!(index, index_context);

    server.post("/", middleware!{ |req, res|
        let form_data = try_with!(res, req.form_body());
        let mut _id_num: u32 = 0;
        let message = Message{
            _id: _id_num,
            body: form_data.get("message").unwrap_or("No Text").to_string(),
            email: form_data.get("email").unwrap_or("No Email").to_string()
        };
        _id_num = _id_num + 1;
        let ninetytwo_db = Connection::connect("postgres://postgres@ninetytwo.ml:5432/ninetytwo", SslMode::None).unwrap();
        ninetytwo_db.execute("INSERT INTO message (body, email) VALUES ($1, $2);",
                 &[&message.body, &message.email]).unwrap();


        println!("{}\n{}", form_data.get("message").unwrap_or("nonne"), form_data.get("email").unwrap_or("nonne"));
        let mut test_context = Context::new();
        test_context.add("projects", &projects);
        let html: String = templates.render("index.html", test_context).unwrap();
        html
    });

    server.listen("0.0.0.0:3000");


}

#[macro_use] extern crate nickel;
extern crate tera;
extern crate postgres;

use postgres::{Connection, SslMode};
use nickel::{Nickel, HttpRouter, StaticFilesHandler, FormBody};

use tera::{Tera, Context};
use std::collections::HashMap;
//use std::str;

fn main() {
    //Initialize nickel instance
    let mut server: Nickel = Nickel::new();

    //Enable static file handling
    server.utilize(StaticFilesHandler::new("/var/www/ninetytwo/static/"));

    //set tera root
    let templates = Tera::new("/var/www/ninetytwo/templates/*");


    //DB Models
    struct Message{
        _id: u32,
        body: String,
        email: String,
    }

    macro_rules! page {
        ($view_name: ident, $context: ident) => (
            let html_path = str::replace(stringify!($view_name), "_", "/");
            let template_name = html_path.to_string() + ".html";
            let html: String = templates.render(template_name.as_str(), $context).unwrap();

            if stringify!($view_name) == "index"{
                server.get("/", middleware!(html.as_str()));
            }
            else{
                let path = str::replace(stringify!($view_name), "_", "/");
                let route = "/".to_string() + path.as_str();

                server.get(route, middleware!(html.as_str()));
            }
        )
    }

    //Routing

    //Home Page
    let mut index_data = HashMap::new();
    index_data.insert("name", "ninetytwo");
    index_data.insert("description", "The source code for this website.");
    index_data.insert("langs", "HTML | CSS | JavaScript | Rust");
    index_data.insert("link", "https://github.com/ninetytwo/ninetytwo.ml");
    let projects = [index_data];
    let mut index_context = Context::new();
    index_context.add("projects", &projects);
    page!(index, index_context);

    server.post("/", middleware!{ |req, res|
        let form_data = try_with!(res, req.form_body());
        let mut _id_num: u32 = 0;
        let message = Message{
            _id: _id_num,
            body: form_data.get("message").unwrap_or("No Text").to_string(),
            email: form_data.get("email").unwrap_or("No Email").to_string()
        };
        _id_num = _id_num + 1;
        let ninetytwo_db = Connection::connect("postgres://postgres@ninetytwo.ml:5432/ninetytwo", SslMode::None).unwrap();
        ninetytwo_db.execute("INSERT INTO message (body, email) VALUES ($1, $2);",
                 &[&message.body, &message.email]).unwrap();


        println!("{}\n{}", form_data.get("message").unwrap_or("nonne"), form_data.get("email").unwrap_or("nonne"));
        let mut test_context = Context::new();
        test_context.add("projects", &projects);
        let html: String = templates.render("index.html", test_context).unwrap();
        html
    });

    server.listen("0.0.0.0:3000");


}
