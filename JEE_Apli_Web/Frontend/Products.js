import React, { useEffect, useState } from "react";
import "./Products.css";
import history from "./history";
import { useLocation } from "react-router-dom";

function Products(props) {
  const [listCards, setListCards] = useState([]);
  const location = useLocation();
  const [totalPrice, setTotalPrice] = useState(0);
  const idUser = 1;
  const [totalCount, setTotalCount] = useState(0);
  const idPanier = 1;
  const [isCartOpen, setIsCartOpen] = useState(false);
  const [productsData, setProductsData] = useState(null);
  const [products, setProducts] = useState([]);
  //const userId = props.location.state.id;
  const userId = location?.state?.id;
 // const products = [];

  /*useEffect(() => {
    fetch ("http://localhost:8080/backend/InitialiserProduits",{
        method : "post",
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
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
  })*/

useEffect(() => {
    fetch("http://localhost:8080/backend/GetProduits", {
        method : "get",
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        })
        .then((response) => response.json())
        .then((data) => {
            setProductsData(data);
            /*products = [
                {
                    id: productsData["id"],
                    name: productsData["nom"],
                    image: 'prod1.jpg',
                    price: productsData["prix"]
                },
                {
                    id: productsData[1].id,
                    name: productsData[1].id,
                    image: 'prod2.jpg',
                    price: productsData[0].prix
                }
            ];*/
            const products = data.map(product => {
                return {
                    id: product.id,
                    nom: product.nom,
                    prix: product.prix,

                };
            });
            setProducts(products);
        })
        .catch(error => console.log(error));
}, []);

  
  useEffect(() => {
    initApp();
  }, [productsData]);

  useEffect(() => {
    reloadCard();
  }, [listCards]);

  function initApp() {
    if (productsData) {
        const newCards = products.map((product) => ({
            ...product,
            quantity: 0,
        }));
        setListCards(newCards);
    }

  }

  function reloadCard() {
    let count = 0;
    let totalPrice = 0;

    listCards.forEach((value) => {
      totalPrice = totalPrice + value.prix * value.quantity;
      count = count + value.quantity;
    });

    setTotalPrice(totalPrice);
    setTotalCount(count);
  }

  function changeQuantity(key, quantity) {
    if (quantity === 0) {
      const updatedCards = [...listCards];
      updatedCards[key].quantity = 0;
      updatedCards[key].prix = 0;
      setListCards(updatedCards);
    } else {
      const updatedCards = [...listCards];
      updatedCards[key].quantity = parseInt(quantity);
      updatedCards[key].prix = updatedCards[key].quantity * products[key].prix;
      setListCards(updatedCards);
    }
  }

  function addToCard(key) {
    const updatedCards = [...listCards];
    const updatedProduct = { ...updatedCards[key] };
    updatedProduct.quantity += 1;
    updatedProduct.prix = updatedProduct.quantity * products[key].prix;
    updatedCards[key] = updatedProduct;
    setListCards(updatedCards);

  }

  function handlePay(index) {
    addToCard(index);
    const updatedListCards = [...listCards];
    const selectedProduct = updatedListCards[index+1];

    fetch("http://localhost:8080/backend/AjouterProduit/"+idPanier+"/"+(index+1)+"/"+idUser, {
        method: "put",
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        }
        })
    };

  function supprimer(index, quantity) {
    changeQuantity(index, quantity - 1)
    fetch("http://localhost:8080/backend/SupprimerProduit/"+idUser+"/"+(index+1), {
      method: "DELETE",
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(null)
    })
  };

  function ajouter(index, quantity) {
    changeQuantity(index, quantity + 1)
    fetch("http://localhost:8080/backend/AjouterProduit/"+idPanier+"/"+(index+1)+"/"+quantity, {
      method: "put",
      headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
      }
      })
  };

  function valider() {
    history.push("/Paiement");
    fetch("http://localhost:8080/backend/Valider", { 
            method: "post",
            headers: {
                'Accept' : 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                /*personne: personne,
                etatCommande: "Confirmée",
                produits: [{produit}],*/
            })
        })
  }


  return (
    <div className="container">
      <header id="header-Products">
        <div className="shopping" onClick={() => setIsCartOpen(true)}>
          <img src="shopping.svg" alt="Shopping Cart" />
          <span className="quantity">{totalCount}</span>
        </div>
      </header>

      <div className="list">
        {products.map((product, index) => (
          <div className="item" key={index}>
            <img src={'prod'+(index+1)+'.jpg'} alt={product.nom} />
            <div className="title">{product.nom}</div>
            <div className="price">{product.prix.toLocaleString()}</div>
            <button onClick={() => handlePay(index)}>Add To Cart</button>
          </div>
        ))}
      </div>

      {isCartOpen && (
        <div className="card">
          <h1>Cart</h1>
          <ul className="listCard">
            {listCards.map((value, key) => (
              <li key={key}>
                <div>
                  <img src={'prod'+(key+1)+'.jpg'} alt={value.nom} />
                </div>
                <div>{value.nom}</div><br></br>
                <div>{value.prix}</div>
                <div>
                  <button
                    onClick={() => supprimer(key, value.quantity)}
                  >
                    -
                  </button>
                  <div className="count">{value.quantity}</div>
                  <button
                    onClick={() => ajouter(key, value.quantity)}
                  >
                    +
                  </button>
                </div>
              </li>
            ))}
          </ul>
          <div className="validerPaiement">
            <button type="button" onClick={() => valider()}>{totalPrice.toLocaleString()}</button>
            <button type="button" onClick={() => setIsCartOpen(false)}>Close</button>
          </div>
        </div>
            )}
    </div>
  );
}

export default Products;


