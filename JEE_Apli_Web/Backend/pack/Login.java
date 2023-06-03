package pack;

import javax.persistence.*;

@Entity
public class Login {


	// on identifie un login par l'adresse mail associée
	@Id
	private String email;
	private String mdp;
	private int idLogin;
	@OneToOne
	private Personne personne;

	public Login() {
	}

	public String getEmail() {
		return email;
	}
	public void setEmail(String email) {
		this.email = email;
	}
	public String getMdp() {
		return mdp;
	}
	public void setMdp(String mdp) {
		this.mdp = mdp;
	}
	public int getIdLogin() {
		return idLogin;
	}
	public void setIdLogin(int idLogin) {
		this.idLogin = idLogin;
	}

	public Personne getPersonne() {
		return personne;
	}

	public void setPersonne(Personne personne) {
		this.personne = personne;
	}


	
}
