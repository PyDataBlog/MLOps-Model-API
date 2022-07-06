import React, {useEffect} from "react";
import {HomeView} from "./Home";
import Resume from "./Resume";
import {NavLink, Route} from "react-router-dom";
import Contact from "./Contact";

type RouteDef = {
  title: string;
  path: string;
  component: React.ReactNode
}

const routeDefs: RouteDef[] = [
  {
    title: "Home",
    path: "/",
    component: HomeView
  },
  {
    title: "Resume",
    path: "/resume",
    component: Resume
  },
  {
    title: "Contact",
    path: "/contact",
    component: Contact
  }
]

export const routes = routeDefs.map(rd => {
  return <Route
    key={rd.title}
    exact
    path={rd.path}
    component={() => {
      useDocumentTitle(rd.title);
      return <rd.component/>;
    }}
  />;
});

const navLinks = routeDefs.map(rd => {
  return <NavLink key={rd.title} exact to={rd.path} className="px-1" activeClassName="border-b border-indigo-400 text-indigo-500">
      {rd.title}
    </NavLink>
});


export default function Navigation() {
  return <div className="flex flex-row justify-center gap-2 text-xl mb-8">
    {navLinks}
  </div>
}

function useDocumentTitle(title: string): void {
  useEffect(() => {
    window.document.title = (title ? `${title} - mattbague` : "mattbague")
  }, [title]);
}