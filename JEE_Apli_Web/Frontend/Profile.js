import React, { useEffect, useState } from "react";
import './Profile.css';
import { useLocation } from "react-router-dom";
import { json } from "react-router-dom";

function Profile(props) {
    const location = useLocation();
    const userId = location?.state?.id;
    const id = 1;
    const [userData, setUserData] = useState(null);


    useEffect(() => {
        fetch("http://localhost:8080/backend/GetPersonne/"+id, {
            method : "get",
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            })
            .then((response) => response.json())
            .then((data) => setUserData(data))
            .catch(error => console.log(error));
    }, [id]);


    function handleClick() {
        //props.changeForm("SignUp");
        
    }

    return (
        <body>
            <div className="container">
                <div className="main">
                    <div className="topbar">
                        <a href="">Ask Us</a>
                        <a href="">Settings</a>
                        <a href="">Log Out</a>
                    </div>
                    <div className="row">
                        <div className="sard text-center sidebar">
                            <div className="sard-body">
                                <img src="profile.jpg" class="rounded-circle" width="150"></img>
                                {userData && (
                                    <div className="mt-3">
                                        <h3> {userData["nom"]} {userData["prenom"]}</h3>
                                        <a href="">Edit</a>
                                        <a href="">Log Out</a>
                                    </div>
                                )}
                            </div>
                        </div>
                        <div className="col-md-8 mt-1">
                            <div className="sard mb-3 content">
                                <h1 className="m-3 pt-3">About</h1>
                                {userData && (
                                    <div className="sard-body">
                                        <div className="row">
                                        </div>
                                        <hr />
                                        <div className="row">
                                            <div className="col-md-3">
                                                <h5>Email</h5>
                                            </div>
                                            <div col-md-9 text-secondary>
                                            {userData["email"]}
                                            </div>
                                        </div>
                                        <hr />
                                        <div className="row">
                                            <div className="col-md-3">
                                                <h5>Phone</h5>
                                            </div>
                                            <div className="col-md-9 text-secondary">
                                                {userData["numTel"]}
                                            </div>
                                        </div>
                                        <div className="row">
                                            <div className="col-md-3">
                                                <h5>Billing Address</h5>
                                            </div>
                                            <div className="col-md-9 text-secondary">
                                                {userData["adresse"]}
                                            </div>
                                        </div>
                                    
                                </div>
                                )}
                            </div>
                            <div className="sard mb-3 content">
                                <h1 className="m-3">Recent Purchases</h1>
                                <div className="sard-body">
                                    <div className="row">
                                        <div className="col-md-3">
                                            <h5>Product Name</h5>
                                        </div>
                                        <div class="col-md-9 text-secondary">
                                            Product description
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </body>
        
    );
}
export default Profile;