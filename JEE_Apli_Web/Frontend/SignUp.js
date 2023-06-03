import React, {Component} from "react";
import { useState } from "react";
import './SignUp.css';
import history from "./history";

function SignUp() {
    const [nom, setNom] = useState("");
    const [prenom, setPrenom] = useState("");
    const [email, setEmail] = useState("");
    const [mdp, setMdp] = useState("");
    const [numTel, setNumTel] = useState("");
    const [adresse, setAdresse] = useState("");
    const handleSubmit = (event) => {
        event.preventDefault();
        /*const data = {
            nom : nom,
            prenom : prenom,
            email : email,
            mdp : mdp,
            panier : null
        };*/
        fetch("http://localhost:8080/backend/AjouterPersonne", {
            method: 'post',
            headers: {
                'Accept' : 'application/json',
                'Content-Type' : 'application/json'
            },
            body: JSON.stringify({
                nom: nom,
                prenom: prenom,
                email: email,
                mdp: mdp,
                numTel: numTel,
                adresse: adresse,
            })
        })
        .then(response => {
            fetch("http://localhost:8080/backend/GetPersonneId/"+nom+"/"+prenom, {
                method:"get",
                headers: {
                    'Accept' : 'application/json',
                    'Content-Type' : 'application/json'
                }
            }).then(response => response.json()).then((data) =>
                fetch("http://localhost:8080/backend/AjouterLogin",{
                    method: "post",
                    headers: {
                        'Accept' : 'application/json',
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify({
                        email:email,
                        mdp:mdp,
                        idLogin: data[0]["id"]
                    })
                }).then( (response) => {

                }));
            //props.changeForm("Login");
            history.push('/Login');
        })
        .catch(error => console.error(error));
    };
    return (
        <div className="signup">
            <header className="signup-header">
                <p1>
                Créer un nouveau compte
                </p1>
                <input value={nom} onChange={(e) => setNom(e.target.value)} type="text" placeholder="Nom" Component="FormInput"></input>
                <input value={prenom} onChange={(e) => setPrenom(e.target.value)} type="text" placeholder="Prénom" Component="FormInput"></input>
                <input value={email} onChange={(e) => setEmail(e.target.value)} type="email" placeholder="Email" Component="FormInput"></input>
                <input value={mdp} onChange={(e) => setMdp(e.target.value)} type="password" placeholder="Mot de passe" Component="FormInput"></input>
                <input value={numTel} onChange={(e) => setNumTel(e.target.value)} type="text" placeholder="Numéro de téléphone" Component="FormInput"></input>
                <input value={adresse} onChange={(e) => setAdresse(e.target.value)} type="text" placeholder="Adresse" Component="FormInput"></input>
                <button type="submit" placeholder="Valider" Component="FormButton" onClick={handleSubmit}>Valider</button>
            </header>
        </div>
    )
}

export default SignUp;