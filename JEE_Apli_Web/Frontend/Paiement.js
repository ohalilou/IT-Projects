import React, { useState } from "react";
import './Paiement.css';
import history from "./history";
import Accueil from "./Accueil";

function Paiement(props) {
    const [donneesPaiement,setDonneesPaiement] = useState({nom:'',
    prenom:'',
    email:'',
    adresse:'',});
    const [numCarte,setNumCarte] = useState("");
    const [dateExpiration,setDateExpiration] = useState("");
    const [cvv,setCvv] = useState("");
    const id = 1;
    const [estValide,setEstValide] = useState(false);
    const [estConfirme,setEstConfirme] = useState(false); // pour confirmer la commande
    const [personne,setPersonne] = useState(null);
    const [panier,setPanier] = useState(null);
    

    const handleSubmit = (event) => {
      event.preventDefault(); {

        fetch("http://localhost:8080/backend/GetPersonne/"+id, {
          method: "get",
          headers: {
            'Accept' : 'application/json',
            'Content-Type': 'application/json'
          },
        })
        .then((response) => response.json())
        .then((data) => {setPersonne(data);
        /*)
        /*.catch(error => console.log(error));*/

        fetch("http://localhost:8080/backend/GetPanier/"+id, {
          method: "get",
          headers: {
            'Accept' : 'application/json',
            'Content-Type': 'application/json'
          },
        })
        .then((response) => response.json())
        .then((data) => {setPanier(data);
        /*)
        .catch(error => console.log(error));*/


        fetch("http://localhost:8080/backend/ValiderPaiement/"+(id+1)+"/"+data.id, { 
            method: "put",
            headers: {
                'Accept' : 'application/json',
                'Content-Type': 'application/json'
            }
        }).then( (response) => {
            // afficher une page confirmation 
            console.log('Done')
        })
        .catch((error) => console.log(error));
      })
      .catch((error) => console.log(error));
      history.push({pathname: "/Accueil", state: {id: data.email}});
    })
    .catch((error) => console.log(error));
    };
  };

    return (
<div className="container">
  <form action="">
    <div className="row">
      <div className="col">
        <h3 className="title">Billing</h3>
        <div className="inputBox">
          <span>full name :</span>
          <input type="text" placeholder="Jean Jaurès" />
        </div>
        <div className="inputBox">
          <span>Email :</span>
          <input type="email" placeholder="example@example.com" />
        </div>
        <div className="inputBox">
          <span>Address :</span>
          <input type="text" placeholder="room - street - district" />
        </div>
        <div className="inputBox">
          <span>City :</span>
          <input type="text" placeholder="Toulouse" />
        </div>
        <div className="flex">
          <div className="inputBox">
            <span>Country :</span>
            <input type="text" placeholder="France" />
          </div>
          <div className="inputBox">
            <span>Postal code :</span>
            <input type="text" placeholder="XXXXX" />
          </div>
        </div>
      </div>

      <div className="col">
        <h3 className="title">Payment</h3>
        <div className="inputBox">
          <span>Cards accepted :</span>
          <img src="card_img.png" alt="" />
        </div>
        <div className="inputBox">
          <span>Name on card :</span>
          <input type="text" placeholder="mr. Jean Jaurès" />
        </div>
        <div className="inputBox">
          <span>Credit card number :</span>
          <input type="number" placeholder="XXXX-XXXX-XXXX-XXXX" />
        </div>
        <div className="inputBox">
          <span>Expiry month :</span>
          <input type="text" placeholder="January" />
        </div>
        <div className="flex">
          <div className="inputBox">
            <span>Expiry date :</span>
            <input type="number" placeholder="mmyyyy" />
          </div>
          <div className="inputBox">
            <span>CVV :</span>
            <input type="text" placeholder="XXX" />
          </div>
        </div>
      </div>
    </div>

    <input type="submit" value="Proceed to checkout" className="submit-btn" onClick={handleSubmit}/>
  </form>
</div>


    )
}
export default Paiement;