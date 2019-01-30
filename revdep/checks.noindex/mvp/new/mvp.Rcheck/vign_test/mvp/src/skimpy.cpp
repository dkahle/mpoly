// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#define container vector         // Could be 'vector' or 'deque' (both work but there may be performance differences)
#define USE_UNORDERED_MAP true   // set to true for unordered_map; comment out to use plain stl map.

#include <Rcpp.h>
#include <cmath>

#include <string.h>
#include <iostream>
#include <unordered_map>
#include <vector>
#include <deque>
#include <utility>
#include <iterator>

using namespace std;
using namespace Rcpp; 

typedef container <signed int> mypowers;  // a mypowers object is a container [vector or deque] of signed integers (the powers of the variables)
typedef container <string> mynames;  // a mynames object is a container [vector or deque] of strings...

typedef map <string, signed int> term; //... and a 'term' object is a map from a string object to an integer; thus a^2 b^3 is 'a' -> 2, 'b' -> 3
typedef map <term, double> mvp;  // ... An 'mvp' object (MultiVariatePolynomial) is a map from a term object to a double.

typedef map <string, double> subs; // A 'subs' object is a map from a string object to a real value, used in variable substitutions; thus a=1.1, b=1.2 is the map {'a' -> 1.1, 'b' -> 2.2}

typedef map <string, mvp> subs_mvp; // A 'subs_mvp' object is a map from a string object to a mvp  object, used in variable  substitutions; thus a=1+3xy^2,
                                    // b=7tt^3 is the map {'a' ->  {emptymap -> 1, {x -> 1, y -> 2} -> 3}, 'b' -> {{tt -> 3} ->  7}}
                                    // (not currently used but might be useful for things like xy^2 /. {x -> 1+a, y -> 1+b}

mvp zero_coefficient_remover(const mvp &X){
    mvp out;
    for(mvp::const_iterator it=X.begin() ; it != X.end() ; ++it){
        const term t = it->first;
        const double coef = it->second;
        if(coef != 0){
            out[t] += coef;
        }
    }
    return out;
}

term zero_power_remover(const term &t){
    term out;
    for(term::const_iterator it=t.begin() ; it != t.end() ; ++it){
        const string var = it->first;
        const int power = it->second;
        if(power != 0){
            out[var] += power;
        }
    }
    return out;
}
    


List retval(const mvp &X){   // takes a mvp object and returns a mpoly-type list suitable for return to R
    unsigned int i,j;
    mvp::const_iterator it;
    term::const_iterator ic;
    term oneterm;
    
    unsigned int n=X.size();
    List namesList(n), powerList(n);
    NumericVector coeff_vec(n);

    for(it = X.begin(), i=0 ; it != X.end() ; ++it, i++){
        oneterm = it->first; // oneterm is a 'term' object, a map from strings to integers
        const unsigned int r = oneterm.size(); // 'r' is the number of variables in oneterm; thus 5x^2*y*z^6 has r=3
        
        CharacterVector names(r); 
        IntegerVector powers(r);

    for(ic = oneterm.begin(), j=0 ; ic != oneterm.end() ; ++ic, ++j){
        names[j] = (string) ic->first;
        powers[j] = (int) ic->second;
    }
        
        namesList[i] = names;
        powerList[i] = powers;
        coeff_vec[i] = (double) it->second;
    }  // 'it' loop closes


    return List::create(Named("names") = namesList,
                        Named("power") = powerList,
                        Named("coeffs") = coeff_vec
                        );
}
    
mvp prepare(const List allnames, const List allpowers, const NumericVector coefficients){  // the entry of each element is the coefficient.
    mvp out;
    term oneterm;
    const unsigned int n=allnames.size();  // n = number of terms (each term has one coefficient)

    for(unsigned int i=0 ; i<n ; i++){  // need to use int i because we are iterating through a list
        unsigned int j; // scope of j should extend beyond the for(j) loop but not the i loop
        SEXP jj = allnames[i]; 
        Rcpp::CharacterVector names(jj);

        SEXP kk = allpowers[i]; 
        Rcpp::IntegerVector powers(kk);

        const unsigned int r=names.size();
             
        // first make a term:
        oneterm.clear();
        for(j=0; j<r; j++){ 
            oneterm[(string) names[j]] += powers[j];  // NB "+=", not "=" because repeated variable names' powers should add [eg x^2*y*x = x^3*y]
        }
       
        // now clear out any variable with a zero power:
        oneterm = zero_power_remover(oneterm);

        // now map the term to its coefficient: 
        if(coefficients[i] != 0){ // only consider terms with a nonzero coefficient
            out[oneterm]  += coefficients[i];
        }
    } // i loop closes

    // now clear out any term with zero coefficient:
    return zero_coefficient_remover(out);
}

mvp product(const mvp X1, const mvp X2){
    mvp out;
    term t1new,t2;
    
    for(mvp::const_iterator it1=X1.begin() ; it1 != X1.end() ; ++it1){
        const term t1=it1->first;
        const double c1=it1->second; // coefficient
        for(mvp::const_iterator it2=X2.begin() ; it2 != X2.end() ; ++it2){
            t1new = t1;// we will modify t1new by adding stuff to it
            t2=it2->first;
            for(term::const_iterator is=t2.begin() ; is != t2.end() ; ++is){
                t1new[is->first] += is->second;  // add the powers of the variables in the two terms
            }  // is loop closes
            
            t1new = zero_power_remover(t1new);
            const double c2=it2->second; // coefficient of t2
            out[t1new] += c1*c2;  // NB inside the it2 loop

        }  //it2 loop closes
    } // it1 loop closes
    return zero_coefficient_remover(out);
}

mvp sum(const mvp X1, const mvp X2){
    mvp out=X1;
    
    for(mvp::const_iterator it2=X2.begin() ; it2 != X2.end() ; ++it2){
            out[it2->first] += it2->second;
    }
    return zero_coefficient_remover(out);
}

mvp power(const mvp X, unsigned int n){
    mvp out; // empty mvp object is the zero polynomial; X^0 is managed in R
    if(n<1){throw std::range_error("power cannot be <1");} 
    if(n==1){
        return X;
    } else {
        out = X; 
        for( ; n>1; n--){
            out = product(X,out);
        }
    }
    return out;
}

mvp deriv(const mvp X, const string v){// differentiation: dX/dv, 'v' a single variable
    mvp out;

    for(mvp::const_iterator it=X.begin() ; it != X.end() ; ++it){
        term t=it->first;
        const signed int power = t[v];       
        t[v]--; // power reduces by one (maybe to zero...)
        t = zero_power_remover(t); // (...which is why we call zpr())
        out[t] = (it->second)* power ;  // coefficient multiplies by power
    }
    return zero_coefficient_remover(out); // eliminates terms with no v
}

// [[Rcpp::export]]
List simplify(const List &allnames, const List &allpowers, const NumericVector &coefficients){
    return retval(prepare(allnames,allpowers,coefficients));
}

// [[Rcpp::export]]
List mvp_prod(
              const List &allnames1, const List &allpowers1, const NumericVector &coefficients1,
              const List &allnames2, const List &allpowers2, const NumericVector &coefficients2
              ){

    return retval(
                  product(
                          prepare(allnames1,allpowers1,coefficients1),
                          prepare(allnames2,allpowers2,coefficients2)
                          )
                  );
}

// [[Rcpp::export]]
List mvp_add(
              const List &allnames1, const List &allpowers1, const NumericVector &coefficients1,
              const List &allnames2, const List &allpowers2, const NumericVector &coefficients2
              ){

    return retval(
                  sum(
                      prepare(allnames1,allpowers1,coefficients1),
                      prepare(allnames2,allpowers2,coefficients2)
                      )
                  );
}

// [[Rcpp::export]]
List mvp_power(
              const List &allnames, const List &allpowers, const NumericVector &coefficients,
              const NumericVector &n
              ){
    return retval(power(prepare(allnames,allpowers,coefficients), n[0]));
}

// [[Rcpp::export]]
List mvp_deriv(
              const List &allnames, const List &allpowers, const NumericVector &coefficients,
              const CharacterVector &v    // v a vector of symbols to be differentiated WR to.
               ){

    const mvp X = prepare(allnames, allpowers, coefficients);
    mvp out = X;
    const unsigned int n=v.size();
     
    for(unsigned int i=0 ; i<n ; i++){
        out = deriv(out, (string) v[i]);
    }
    return retval(out);
}

// [[Rcpp::export]]
List mvp_substitute(
              const List &allnames, const List &allpowers, const NumericVector &coefficients,
              const CharacterVector &v, const NumericVector &values
    ){
    mvp X = prepare(allnames, allpowers, coefficients);
    subs s;  // "subs" is a substitution object, e.g. {x -> 3, y -> 4, zzd -> 10.1}
    const unsigned int n=v.size();

    for(unsigned int i=0 ; i<n ; i++){
      s[(string) v[i]] = values[i];
    }

    subs::const_iterator i;
    mvp::const_iterator j;
    term::iterator it;

    mvp Xnew;    
    for(i = s.begin() ; i != s.end() ; ++i){     // Iterate through the substitution object s; e.g. {x=1.1, b=5.5};  i->first = "x" and  i->second = 1.1
        Xnew.clear();                            // Empty mvp object to take substituted terms
        for(j = X.begin() ; j != X.end() ; ++j){ // Iterate through  X; e.g. j->first = {"x" -> 3, "ab" -> 5} [that is, x^3*ab^5] and j->second=2.2 [that is, 2.2 x^3*ab^5]
            term t = j->first;                   // "t" is a single term of X, eg {"x" -> 3, "ab" -> 5} [that is, x^3*ab^5]
            const double coeff = j->second;      // "coeff" is the coefficient corresponding to that term (a real number)
	    it = t.find(i->first);               // Now, search the symbols in the term for one that matches the substitution symbol, e.g. it->first = {"x"}
            if(it == t.end()){                   // if(no match)...
                Xnew[t] = coeff;                 // ...then include term t and coeff unchanged in Xnew
	    } else {                             // else a match found.  If so, we want to effect 3x^2*y^5 /. {x -> 2} giving 12*y^3 [mathematica notation];  do three things:
              const signed int n=it->second;     // (1) extract the power, n, *before* erasing the iterator;
	      t.erase(it);                       // (2) Set the power of the matched symbol to zero (in t); and
	      Xnew[t] +=                         // (3) Add a new element to Xnew with term (updated) t...
              coeff*pow(i->second,n);            // ... and coefficient coeff*<var>^n using the saved value of n; note use of "+=" in case there is another term the same
            }                                    // if(match found) closes
	}                                        // j loop closes: go on to look at the next element of X
        X = Xnew;                                // update X to reflect changes
    }                                            // i loop closes: go on to consider the next element of substitution object s 
    return(retval(X));                           // return a pre-prepared list to R
}                                                // function mvp_substitute() closes

// [[Rcpp::export]]
List mvp_substitute_mvp(
              const List &allnames1, const List &allpowers1, const NumericVector &coefficients1, // original mvp
              const List &allnames2, const List &allpowers2, const NumericVector &coefficients2, // mvp to substitute with
              const CharacterVector &v                                                           // symbol to substitute for
    ){

    const mvp X = prepare(allnames1, allpowers1, coefficients1);  // original mvp object
    const mvp Y = prepare(allnames2, allpowers2, coefficients2);  // mvp object to substitute v for


    mvp::const_iterator i;
    term::iterator it;
    
    mvp Xnew,Xtemp;

    for(i = X.begin() ; i != X.end() ; ++i){ // Iterate through  X; e.g. i->first = {"x" -> 3, "ab" -> 5} [that is, x^3*ab^5] and i->second=2.2 [that is, 2.2 x^3*ab^5]
        term t = i->first;                   // "t" is a single _term_ of X, eg {"x" -> 3, "ab" -> 5} [that is, x^3*ab^5]
        const double coeff = i->second;      // "coeff" is the coefficient corresponding to that term (a real number)
        it = t.find((string) v[0]);          // Now, search the symbols in term "t" for one that matches the substitution symbol
        if(it == t.end()){                   // if(no match)...
            Xnew[t] += coeff;                // ...then include term t and coeff unchanged in Xnew
        } else {                             // else a match found.  If so, we want to effect things like 3x^2*y^5z /. {t -> 1+a} giving 3x^2*(1+a)^5*z
            const signed int psubs=it->second; // "psubs" is "the power of the variable being substituted for"
            if(psubs<0){throw std::range_error("negative powers cannot be substituted for");}
            t.erase(it);                     // Remove the matched symbol from t
            Xtemp.clear();                   // Clear Xtemp, now the zero polynomial
            Xtemp[t] = coeff;                // Algebraically, Xnew = coeff*term-without-match
            Xtemp = product(Xtemp, power(Y, psubs));  // this is the "meat" of the function
            Xnew = sum(Xnew,Xtemp);          // Take cumulative sum
        }                                    // if(match found) closes
    }                                        // i loop closes: go on to consider the next element of X
    return(retval(Xnew));                    // return a pre-prepared list to R
}                                            // function mvp_substitute() closes

