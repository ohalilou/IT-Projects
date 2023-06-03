package pack;
import java.util.Collection;

import javax.ejb.LocalBean;
import javax.ejb.Singleton;
import javax.persistence.*;
import javax.ws.rs.*;
import org.w3c.dom.Entity;
import javax.persistence.*;
@Singleton
@Path("/")
public class Facade {
    @PersistenceContext    
    private EntityManager em ;

    public Facade() {
    	/*Produit p1 = new Produit("HP Chromebook 14B", 699.00, "Ordinateur portable ...");
    	Produit p2 = new Produit("HP Laptop 14", 699.00, "Ordinateur portable ...");
    	Produit p3 = new Produit("HP Ordinateur Portable 15S", 449.00, "Ordinateur portable ...");
    	Produit p4 = new Produit("Surface Pro 7+", 929.00, "Ordinateur portable ...");
    	Produit p5 = new Produit("Apple Macbook Pro 15\" Retina", 699.00, "Ordinateur portable ...");
    	Produit p6 = new Produit("Dell Inspiron 15", 699.00, "Ordinateur portable ...");
    	Produit p7 = new Produit("Lenovo Legion 5", 799.00, "Ordinateur portable ...");
    	em.persist(p1);
    	em.persist(p2);
    	em.persist(p3);
    	em.persist(p4);
    	em.persist(p5);
    	em.persist(p6);
    	em.persist(p7);*/
    }

    @POST
    @Path("/AjouterPersonne")
    @Consumes({"application/json"})
    public void ajouterPersonne(Personne p) {
    	Panier panier = new Panier();
    	p.setPanier(panier);
    	em.persist(panier);
        em.persist(p);
    }

    @GET
    @Path("/GetPersonneId/{nom}/{prenom}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public Collection<Personne> GetPersonneId(@PathParam("nom") String nom, @PathParam("prenom") String prenom) {
    	return em.createQuery("select personne from Personne personne where personne.nom='"+nom+"' and personne.prenom='"+prenom+"'",Personne.class).setMaxResults(1).getResultList();
    }

    @GET
    @Path("/GetId/{email}/{mdp}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public int GetId(@PathParam("email") String email, @PathParam("mdp") String mdp) {
    	int id = 1000;
    	for (Personne p : em.createQuery("select personne from Personne personne where personne.email='"+email+"' and personne.mdp='"+mdp+"'",Personne.class).setMaxResults(1).getResultList()) {
    		id = p.getId();
    	}
    	return id;
    }

    @POST
    @Path("/AjouterLogin")
    @Consumes({"application/json"})
    public void AjouterLogin(Login login) {
    	em.persist(login);
    }

    // méthode de vérification des données de login
    @GET
    @Path("/Login/{e}/{m}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public Boolean verifierLogin(@PathParam("e") String email, @PathParam("m") String mdp) {
    	return em.createQuery("select l from Login l where l.email = '"+email+"' and l.mdp = '"+mdp+"'", Login.class).getResultList().size()!=0;
    }

    /*@GET
    @Path("/ListerPersonne")
    @Produces({"application/json"})
    public Collection<Personne> listerPersonnes(){
        TypedQuery<Personne> req= em.createQuery("select p from  Personne p",Personne.class);
        return req.getResultList();
    } 
    /*/
    /*public Collection<Produit> filtrerProduits() {
    	return em.createQuery("from Produit",Produit.class).getResultList();
    }*/
    
    // récupérer les produits de la base de données
    @GET
    @Path("/GetProduits")
    @Produces({"application/json"})
    public Collection<Produit> GetProducts() {
    	return em.createQuery("from Produit",Produit.class).getResultList();
    }

    // ajouter un produit au panier
    @PUT
    @Path("/AjouterProduit/{idPanier}/{idProduit}/{quantity}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public void ajouterProduit(@PathParam("idPanier") int idPanier,@PathParam("idProduit") int idProduit, @PathParam("quantity") int quantity){
    	/*Personne personne = em.find(Personne.class,id);
    	Produit produit = em.find(Produit.class, idprod);
    	personne.getPanier().ajouterProduit(produit);
        em.merge(personne);*/
    	Produit produit = em.find(Produit.class, idProduit);
    	Panier panier = em.find(Panier.class, idPanier);
    	produit.setPanier(panier);
    	produit.setNombre(quantity);
    	em.merge(produit);
    }

    // supprimer un produit du panier
    @DELETE
    @Path("/SupprimerProduit/{idPersonne}/{idProduit}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public void supprimerProduit(@PathParam("idPersonne") int idPersonne, @PathParam("idProduit") int idProduit) {
    	Personne personne = em.find(Personne.class, idPersonne);
    	Produit produit = em.find(Produit.class,idProduit);
    	produit.setPanier(null);
    	personne.getPanier().supprimerProduit(produit);
    	em.merge(personne);
    }

    // lister les produits ajoutés au panier d'un utilisateur
	@GET
    @Path("/ListerProduits/{id}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public Collection<Produit> listerProduits(@PathParam("id") int idPersonne){
    	return em.createQuery("select p from Produit p JOIN p.paniers JOIN panier.personne pers where pers.id =:id", Produit.class).getResultList();
        //Personne p = em.find(Personne.class,idPersonne);
        //return p.getPanier().getProduits();
    }

	// Afficher la liste des produits selon le filtre appliqué
   /* @GET
    @Path("/Filtrer/{f}/{v}")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    public Collection<Produit> filtrerProduits(@PathParam("f") Filtre filtre, @PathParam("v") String valeur) {
        if (filtre.getType()==(TypeFiltre.PRIX)) {
            return em.createQuery("select p from Produit p where p.prix < :" + valeur, Produit.class).getResultList();
        } else if (filtre.getType()==(TypeFiltre.MARQUE)) {
            return em.createQuery("select p from Produit p where p.marque = :" + valeur, Produit.class).getResultList();
        } else {
            return em.createQuery("select p from Produit p where p.type = :" + valeur, Produit.class).getResultList();
        }
    }
*/
    @GET
    @Path("/GetPersonne/{id}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public Personne GetPersonne(@PathParam("id") int id) {
    	Personne p = em.find(Personne.class, id);
    	if (p == null) {
    		throw new RuntimeException("Utilisateur introuvable");
    	}
    	return p;
    }

    // modifier les données (login) d'un utilisateur
    @POST
    @Path("/ModifierPersonne/{id}/{nom}/{prenom}/{email}/{pswd}")
    @Produces({"application/json"})
    public void modifierPersonne(@PathParam("id") int id, @PathParam("nom") String nom, @PathParam("prenom") String Prenom, @PathParam("email") String nouveauMail, @PathParam("pswd") String nouveauPassword) {
    	Personne p = em.find(Personne.class, id);
    	p.setNom(nom);
    	p.setPrenom(Prenom);
    	p.setEmail(nouveauMail);
    	p.setMdp(nouveauPassword);
    	em.merge(p);
    }

    // valider panier
    @POST
    @Path("/Valider")
    @Consumes({"application/json"})
    public void valider(Commande commande) {
    	em.persist(commande);
    }

    // valider la commande après paiement
    @PUT
    @Path("/ValiderPaiement/{idCommande}/{idPanier}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public void validerPaiement(@PathParam("idCommande") int idCommande, @PathParam("idPanier") int idPanier) {
    	Commande commande = em.find(Commande.class, idCommande);
    	Panier panier = em.find(Panier.class, idPanier);
    	commande.setPanier(panier);
    	commande.setEtatCommande("commande confirmée");
    	em.merge(commande);
    }
    
    @GET
    @Path("/GetProduit/{id}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public Produit GetProduit(@PathParam("id") int id) {
    	Produit p = em.find(Produit.class, id);
    	if (p == null) {
    		throw new RuntimeException("Produit introuvable");
    	}
    	return p;
    }
    
    @GET
    @Path("/GetPanier/{id}")
    @Consumes({"application/json"})
    @Produces({"application/json"})
    public Panier GetPanier(@PathParam("id") int id) {
    	Panier p = em.find(Panier.class, id);
    	if (p == null) {
    		throw new RuntimeException("Panier introuvable");
    	}
    	return p;
    }
    
}