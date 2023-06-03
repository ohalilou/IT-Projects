import React, { Component} from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import Principale from "./Principale";
import SignUp from "./SignUp";
import Login from "./Login";
import About from "./About";
import Products from "./Products";
import Profile from "./Profile";
import Accueil from "./Accueil";
import history from "./history";
import Paiement from "./Paiement";



function AppRoutes() {
    return (
        <Router history = {history}>
            <Routes>
                <Route path="/Principale" element={<Principale />}/>
                <Route path="/SignUp" element={<SignUp />}/>
                <Route path="/Login" element={<Login />}/>
                <Route path="/About" element={<About />}/>
                <Route path="/Products" element={<Products />}/>
                <Route path="/MyCart" element={<Panier />}/>
                <Route path="/MyProfile" element={<Profile />}/>
                <Route path="/Accueil" element={<Accueil />} />
                <Route path="/Paiement" element={<Paiement />} />
                <Route path="/" element={<Principale />}/>

            </Routes>
        </Router>
    )
}
export default AppRoutes;