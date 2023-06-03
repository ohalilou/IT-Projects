import React from "react";
import './About.css';

function About(props) {
    return (
        <div className="toto">
            <div className="heading" >
                <h1>About Us</h1>
                <p>We are four INP-ENSEEIHT students, and we have made this website for 
                you to but all sorts of electronic devices that you desire!</p>
            </div>
            <div className="container">
                <section className="about">
                    <div className="about-content">
                        <h1>This product was delivered with care and diligence!</h1>
                        <p>
                            You can contact the founders via email:
                        </p>
                        <h1>Zakaria Lahmouz : zakaria.lahmouz@etu.toulouse-inp.fr</h1>
                        <h1>Oussama Karmaoui: oussama.karmaoui@etu.toulouse-inp.fr</h1>
                        <h1>Haliloua Othmane: othmane.haliloua@etu.toulouse-inp.fr</h1>
                        <h1>Elalout Ismail  : ismail.elalout@etu.toulouse-inp.fr</h1>
                    </div>
                </section>
            </div>
        </div>
        
    );
}
export default About;