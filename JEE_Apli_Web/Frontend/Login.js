import React, {Component} from "react";
import { useState } from "react";
import './Login.css';
import history from "./history";

function Login() {
    const [email, setEmail] = useState("");
    const [mdp, setMdp] = useState("");
    const [isLogged, setIsLogged] = useState(false);
    const handleSubmit = (event) => {
        event.preventDefault();
        fetch("http://localhost:8080/backend/Login/"+email+"/"+mdp, {
            method : "get",
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            })
            .then( (response) =>  response.json())
            .then( (data) => {
                if (data) {
                    setIsLogged(true);
                } else {
                    setIsLogged(false);
                }
                if (isLogged == true) {
                    //props.changeForm("Accueil");
                    history.push({pathname: "/Accueil", state: {id: email}});
                }
            });   
    };
    return (
        <div className="login">
            <header className="login-header">
                <p1>
                Se connecter à votre compte
                </p1>
                <form onSubmit={handleSubmit}>
                    <input value={email} onChange={(e) => setEmail(e.target.value)} type="email" placeholder= "Adresse mail" Component="FormInput"></input>
                    <input value={mdp} onChange={(e) => setMdp(e.target.value)} type="password" placeholder= "Mot de passe" Component="FormInput"></input>
                    <button type="submit" placeholder="Valider" Component="FormButton" onClick={handleSubmit}>Valider</button>
                </form>
            </header>
        </div>
    )
}

export default Login;