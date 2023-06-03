package pack;

import java.util.Collection;

import javax.persistence.*;

@Entity
public class Carte {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private int id;

	private int carteNum;
    private int expirMonth;
    private int expirYear;
    private int crypto;


	//On peut utiliser une meme carte depuis plusieurs comptes
	// et un compte peut utiliser plusieurs cartes pour le paiement
	@ManyToMany
	Collection<Personne> personnes;


	public int getId() {
		return id;
	}


	public void setId(int id) {
		this.id = id;
	}


	public int getCarteNum() {
		return carteNum;
	}


	public void setCarteNum(int carteNum) {
		this.carteNum = carteNum;
	}


	public int getExpirMonth() {
		return expirMonth;
	}


	public void setExpirMonth(int expirMonth) {
		this.expirMonth = expirMonth;
	}


	public int getExpirYear() {
		return expirYear;
	}


	public void setExpirYear(int expirYear) {
		this.expirYear = expirYear;
	}


	public int getCrypto() {
		return crypto;
	}


	public void setCrypto(int crypto) {
		this.crypto = crypto;
	}


	public Collection<Personne> getPersonnes() {
		return personnes;
	}


	public void setPersonnes(Collection<Personne> personnes) {
		this.personnes = personnes;
	}

	
}
