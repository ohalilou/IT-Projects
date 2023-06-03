import logo from './logo.svg';
import SignUp from './SignUp';
import Principale from './Principale';
import Login from './Login';
import About from './About';
import Products from './Products';
import Accueil from './Accueil';
import Panier from './Panier';
import Profile from './Profile';
import { useState } from "react";
import './App.css';
import AppRoutes from './AppRoutes';

function App() {
  const [currentForm, setCurrentForm] = useState("Principale");
  function changeForm(Form) {
    setCurrentForm(Form);
  }
  return (
    /*<div className="App">
      {currentForm == "Principale" ? (
        <Principale changeForm={changeForm} />
      ) : currentForm == "SignUp" ? (
        <SignUp changeForm={changeForm} />
      ) : currentForm == "About" ? (
        <About changeForm={changeForm} />
      ) : currentForm == "Login" ? (
        <Login changeForm={changeForm} />
      ) :  currentForm == "Products" ? (
        <Products changeForm={changeForm} />
      ) :  currentForm == "Accueil" ? (
        <Accueil changeForm={changeForm} />
      ) :  currentForm == "Profile" ? (
        <Profile changeForm={changeForm} />
      ) : <Panier changeFor={changeForm} /> }
    </div>
    /*<div className="App">
      <Routes />
    </div>*/
   
    <AppRoutes />

  );
}

export default App;
