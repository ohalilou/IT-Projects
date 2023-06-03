import React, { useEffect } from "react";
import './Accueil.css';
import history from "./history";
import { useLocation } from "react-router-dom";

function Accueil(props) {
    const location = useLocation();
    //const userId = props.location.state.id;
    const userId = location?.state?.id;
    useEffect(() => {
        let nbr = 6;
        let n = 0;
        let container = document.getElementById("container");
        let g = document.getElementById("g");
        let d = document.getElementById("d");
        let photoDivs = [];

        container.style.width = (800 * nbr) + "px";

        for (let i = 1; i <= nbr; i++) {
            let div = document.createElement("div");
            div.className = "photo";
            div.style.backgroundImage = `url('./${i}.jpg')`;
            photoDivs.push(div);
            container.appendChild(div);
        }

        function afficherMasquer() {
            if (n === -nbr + 1)
                g.style.visibility = "hidden";
            else
                g.style.visibility = "visible";

            if (n === 0)
                d.style.visibility = "hidden";
            else
                d.style.visibility = "visible";
        }

        g.onclick = function () {
            if (n > -nbr + 1)
                n--;
            container.style.transform = `translate(${n * 800}px)`;
            container.style.transition = "all 0.5s ease";
            afficherMasquer();
        };

        d.onclick = function () {
            if (n < 0)
                n++;
            container.style.transform = `translate(${n * 800}px)`;
            container.style.transition = "all 0.5s ease";
            afficherMasquer();
        };

        afficherMasquer();

        return () => {
            // Cleanup code, if needed
        };
    }, []);

    function handleClick() {
        //props.changeForm("Principale");
        history.push('/Principale');
    }

    function handleClick2() {
       // props.changeForm("Profile");
       history.push('/MyProfile');
       history.push({pathname: "/MyProfile", state: {id: userId}});

    }

    function handleClick3() {
        //props.changeForm("Products");
        history.push('/Products');
    }

    function handleClick4() {
        //props.changeForm("About");
        history.push('/About');
    }



    return (
        <div className="principale">
            <header className="principale-header">
                <p>
                    Electronic Corner
                </p>
                <nav className="bar">
                    <ul>
                    <li><a href="" onClick={handleClick3}>Our products</a></li>
                    <li><a href="" onClick={handleClick2}>My Profile</a></li>
                    <li><a href="" onClick={handleClick}>Sign out</a></li>
                    <li><a href="" onClick={handleClick4}>AboutUs</a></li>
                    </ul>
                </nav>
            </header>
            <div id="carrousel">
                <div id="container"></div>
                <img src="./copy.png" className="bouton" id="d" alt="Copy" />
                <img src="./copy.png" className="bouton" id="g" alt="Copy" />
            </div>
        </div>
    );
}

export default Accueil;